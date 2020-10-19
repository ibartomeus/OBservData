
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(readxl)

dir_ini <- getwd()

##########################
#Data: Jauker_2006
##########################

#data_raw <- read_csv("Individual CSV/Jauker_2006.csv")

data_raw <- read_excel("Individual CSV/Oilseedrape_Jauker_03.xls")


# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]

# Add month of sampling
data_raw_without_NAs$month_of_study <- as.numeric(format(as.Date(data_raw_without_NAs$date, format="%Y/%m/%d"),"%m"))


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = "Frank_Jauker_Brassica_napus_Germany_2006",
  site_id = data_raw_without_NAs$site_ID,
  crop = "Brassica napus",
  variety = NA,
  management = "conventional",
  country = "Germany",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = 2006,
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
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year,yield_units) %>% 
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA


# Fix sampling months

sites <- unique(data.site_aux$site_id)

data_raw_without_NAs$month_of_study

for (i in sites){
  
  data.site$sampling_start_month[data.site$site_id==i] <- 
    data_raw_without_NAs %>% filter(site_ID==i) %>% 
    select(month_of_study) %>% min()
  
  data.site$sampling_end_month[data.site$site_id==i] <- 
    data_raw_without_NAs %>% filter(site_ID==i) %>% 
    select(month_of_study) %>% max()
  
}


#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1007/s10980-009-9331-2"
data.site$Credit  <- "Frank Jauker"
data.site$Email_contact <- "Frank.Jauker@allzool.bio.uni-giessen.de"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw_without_NAs %>%
  select(site_ID, names(data_raw[17:ncol(data_raw)])) %>% rename(site_id=site_ID)

# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


data_raw_gather <- data_raw_obs %>% 
  gather(-site_id,key = "Organism_ID", value = 'Abundance', !contains("site_id")) %>%
  filter(!is.na(Abundance))

data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

data_raw_gather$Guild[grepl("Andrena",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Apis melifera",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "honeybees"
data_raw_gather$Guild[grepl("Bombus",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
data_raw_gather$Guild[grepl("Hylaeus",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Lasioglossum",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Anasimyia",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Eristalis",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Syrphus",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Tropidia",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Sphaerophoria",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Scaeva",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Parhelophilus",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Melangyna",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"
data_raw_gather$Guild[grepl("Melanostoma",data_raw_gather$Organism_ID,ignore.case = FALSE)] <- "syrphids"

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# we established six
# 2,000 m transects for the sampling of wild bees and
# hoverflies
# We conducted five surveys in June and July 2006.
# Wild bees and hoverflies were recorded at point stops
# every 100 m along the transects, resulting in 20
# recordings per transect and 120 recordings for six
# transects per survey

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0,!is.na(Abundance))

#Sampling rounds

sampling <- data_raw_without_NAs %>% group_by(site_ID) %>% count() %>%
  rename(site_id=site_ID) %>% mutate(time=n*30,area=n*20*20)

data_raw_gather <- data_raw_gather %>% left_join(sampling,by="site_id")

insect_sampling <- tibble(
  study_id = "Frank_Jauker_Brassica_napus_Germany_2006",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "netting",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = data_raw_gather$area,
  total_sampled_time = data_raw_gather$time,
  total_sampled_flowers = NA,
  Description = "Six 2000m transects were sampled. Sampling was conducted in a 20 x 20 m area every 100 m. Here results represent samping points with pollinator records from oilseed rape.  At point stops, wild bees and hoverflies were sampled for 20 min using insect nets. There were 5 sampling rounds, so some fields turn up more than once, if oilseed rape was in bloom over more than one sampling round"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Frank_Jauker_Brassica_napus_Germany_2006.csv")
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

# There are     "bumblebees"      "honeybees"       "other_wild_bees" "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other_flies=0, beetles=0, non_bee_hymenoptera=0, lepidoptera=0,
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
percentage_species_morphos <- .9

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="bees and hoverflies")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}


data.site <- data.site %>% left_join(richness_aux,by="site_id")

###############################
# SAMPLING DATA
###############################

data.site <- data.site %>% left_join(sampling,by="site_id")


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
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  richness_restriction = data.site$richness_restriction,
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
  total_sampled_area = data.site$area,
  total_sampled_time = data.site$time,
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
write_csv(field_level_data, "field_level_data_Frank_Jauker_Brassica_napus_Germany_2006.csv")
setwd(dir_ini)

