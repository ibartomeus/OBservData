
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Cunningham_2006
##########################

data_raw <- read_csv("Individual CSV/Cunningham_2006.csv")

# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_",data_raw_without_NAs$Year_of_study),
  site_id = data_raw_without_NAs$site,
  crop = "Brassica napus",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "Australia",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = 10,
  sampling_end_month = 10,
  sampling_year = data_raw_without_NAs$Year_of_study,
  field_size = NA,
  yield= NA,
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
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year,yield_units) %>% 
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA

############################################################

# Convert Latitude/Longitude from degrees min sec to decimal
#34 ° 29 ' 7 ''

chd = substr(data.site$latitude, 4, 4)[1]
chm = substr(data.site$latitude, 8, 8)[1]
chs = substr(data.site$latitude, 12, 13)[1]

cd = char2dms(data.site$latitude,chd=chd,chm=chm,chs=chs)
data.site$latitude <- as.numeric(cd)
#148 ° 34 ' 51 ''
chd = substr(data.site$longitude, 5, 5)[1]
chm = substr(data.site$longitude, 10, 10)[1]
chs = substr(data.site$longitude, 14, 15)[1]

cd = char2dms(data.site$longitude,chd = chd,chm = chm,chs = chs)
data.site$longitude <- as.numeric(cd)
  
#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1016/j.baae.2010.05.001"
data.site$Credit  <- "Anthony D. Arthur, Jin Li, Steve Henry, and Saul A. Cunningham"
data.site$Email_contact <- "saul.cunningham@csiro.au"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(site,round,row,observation_location, names(data_raw[30:ncol(data_raw)]))

# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


data_raw_gather <- data_raw_obs %>% gather(-site,key = "Organism_ID", value = 'Abundance', !contains("site"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# Pollinators were counted in transects 22.5 m long by 3 m
# wide. The entire transects had been covered in 5-8 min each
# 
# The density of inflorescences on each transect was estimated using four 50 cm × 50 cm randomly placed quadrats.
# The average count from the four quadrats was used as a
# possible explanatory variable in analyses.

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0) %>% rename(site_id=site)

#########################
# Add field size and transect pairs from paper (table 1)
# Note that order is mutated a,b,c,d,e,f in table 1 corresponds here with 1,2,3,4,5,6

data_transects <- tibble(site_id=data_raw_obs$site,transects=c(16,9,35,17,11,18),
                         field_size = c(86,62,67,20,34,21)) %>% 
  mutate(total_area = 2*transects*22.5*3,
         total_time = 2*transects*(5+8)/2)

data.site <- data.site %>% left_join(data_transects,by="site_id")

data_raw_gather <- data_raw_gather %>% left_join(data_transects,by="site_id")
data_raw_gather$Abundance <- 2*data_raw_gather$Abundance*data_raw_gather$transects

insect_sampling <- tibble(
  study_id = "Cunningham_2006",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "transects",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = data_raw_gather$total_area,
  total_sampled_time = data_raw_gather$total_time,
  total_sampled_flowers = NA,
  Description = "22.5 m long by 3 m wide transects, 5-8 min each."
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Cunningham_2006.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_raw_gather <-  data_raw_gather %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_raw_gather %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are     "honeybees"     "other_wild_bees"
# "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,lepidoptera=0,
                                          other_flies=0,beetles=0,non_bee_hymenoptera=0,
                                          other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>%
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

# Load our estimation for taxonomic resolution

tax_res <- read_csv("taxon_table_Rader.csv")
#Mutate pollinator labels to match those of taxon table
tax_estimation <- insect_sampling %>% mutate(pollinator=str_replace(pollinator,"_"," ")) %>%
  left_join(tax_res, by="pollinator")
tax_estimation %>% group_by(rank) %>% count()

percentage_species_morphos <- 
  sum(tax_estimation$rank %in% c("morphospecies","species"))/nrow(tax_estimation)


richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

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
  field_size = data.site$field_size.y,
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
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
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
  total_sampled_area = data.site$total_area,
  total_sampled_time = data.site$total_time,
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
write_csv(field_level_data, "field_level_data_Cunningham_2006.csv")
setwd(dir_ini)
