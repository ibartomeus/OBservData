
library(tidyverse)
library(rgdal) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Scheper_2012
##########################

data_raw <- read_csv("Individual CSV/Scheper_2012.csv")

# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

data_raw_without_NAs <-  data_raw_without_NAs %>% 
  separate(latitude,c("zone_UTM","chunk","X_UTM","Y_UTM")," ")

data_raw_without_NAs[, c(9,11:12)] <- sapply(data_raw_without_NAs[, c(9,11:12)], as.numeric)

sputm <- SpatialPoints((data_raw_without_NAs[,11:12]),
                       proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

data_raw_without_NAs$longitude <- NA
data_raw_without_NAs$latitude <- NA

which(names(data_raw_without_NAs)=="longitude")
which(names(data_raw_without_NAs)=="latitude")

data_raw_without_NAs[,72:73] <- spgeo@coords


data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_",data_raw_without_NAs$Year_of_study),
  site_id = data_raw_without_NAs$site,
  crop = "Brassica napus",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "Netherlands",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=data_raw_without_NAs$X_UTM,
  Y_UTM=data_raw_without_NAs$Y_UTM,
  zone_UTM=data_raw_without_NAs$zone_UTM,
  sampling_start_month = 4,
  sampling_end_month = 5,
  sampling_year = data_raw_without_NAs$Year_of_study,
  field_size = NA,
  yield=NA,
  yield_units=NA,
  yield2= NA,
  yield2_units= NA,
  yield_treatments_no_pollinators= NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2= NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant= NA,
  fruit_weight=  NA,
  plant_density= NA,
  seeds_per_fruit= NA,
  seeds_per_plant= NA,
  seed_weight= NA
)

data.site <- data.site_aux %>% 
  group_by(study_id,site_id,crop,variety,management,country,
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year) %>% 
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA

############################################################
#########################
# Adding credit, Publication and contact

data.site$Publication <- NA
data.site$Credit  <- "Jeroen Scheper"
data.site$Email_contact <- "jeroen.scheper@wur.nl"

###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>%
  select(site,sample_num,round,row,observation_location, names(data_raw[30:ncol(data_raw)]))

# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]

# Add site observations
data_raw_g <- data_raw_obs %>% select(-sample_num) %>%
  group_by(site) %>% summarise_all(sum,na.rm=TRUE)

# Gather observations
data_raw_gather <- data_raw_g %>% gather(-site,key = "Organism_ID", value = 'Abundance', !contains("site"))

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# In each field, flower-visiting insects were surveyed
# in two 1 × 150 m transects located at the edge and in the
# interior of the field (>25 m from the field edge). Transects were
# subdivided into three 1 × 50 m plots. In each plot, insects visiting
# crop flowers were collected during a period of 5 min

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0, is.na(Family))

insect_sampling <- tibble(
  study_id = "Scheper_2011",
  site_id = data_raw_gather$site,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "transects",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = 2*150*1,
  total_sampled_time = 2*3*5,
  total_sampled_flowers = NA,
  Description = "Two 150 × 1 m transect per field, 15 min each"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Scheper_2012.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

abundance_aux <- data_raw_gather %>% rename(site_id=site) %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are ""bumblebees"      "honeybees"    "non_bee_hymenoptera"
# "other_wild_bees" "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other_flies=0,lepidoptera=0,beetles=0,
                                          other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>% rename(site_id=site) %>%
  select(site_id,Organism_ID,Abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=Abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}

richness_aux <- abundace_field %>% select(site_id, r_chao)
richness_aux <- richness_aux %>% rename(pollinator_richness=r_chao) %>%
  mutate(richness_estimator_method="Chao1")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = data.site$study_id,
  site_id = data.site$site_id,
  crop = data.site$crop,
  variety = data.site$variety,
  management = data.site$management,
  country = data.site$country,
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=data.site$zone_UTM,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_no_pollinators,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  pollinator_richness = data.site$pollinator_richness,
  richness_estimator_method = data.site$richness_estimator_method,
  abundance = data.site$total,
  ab_honeybee = data.site$honeybees,
  ab_bombus = data.site$bumblebees,
  ab_wildbees = data.site$other_wild_bees,
  ab_syrphids = data.site$syrphids,
  ab_humbleflies= data.site$humbleflies,
  ab_other_flies= data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others = data.site$other,
  total_sampled_area = 2*150,
  total_sampled_time = 2*15,
  visitation_rate_units = NA,
  visitation_rate = NA,
  visit_honeybee = NA,
  visit_bombus = NA,
  visit_wildbees = NA,
  visit_syrphids = NA,
  visit_humbleflies = NA,
  visit_other_flies = NA,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Scheper_2012.csv")
setwd(dir_ini)
