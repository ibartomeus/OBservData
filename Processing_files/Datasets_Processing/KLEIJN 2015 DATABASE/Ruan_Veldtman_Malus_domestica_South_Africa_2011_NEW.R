
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
#library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 84_RuanVeldtmanApple_2011
##########################

data_raw <- read.xlsx("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/84_RuanVeldtmanApple_2011/SA_Apple_FieldTrip2010_JFColville_FOR_RUAN_July2012_EDIT.xlsx",
                       sheet = "Scan Sampling")
data_raw <- as_tibble(data_raw) %>% select(-X1)

# Tranforms date to proper units
data_raw$date <- openxlsx::convertToDate(data_raw$date)
data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$date, format="%Y/%m/%d"),"%m"))


# There should be 10 sites
data_raw %>% group_by(site_id) %>% count()

##############
# Data site
##############

data.site <- data_raw  %>%
  select(site_id,latitude,longitude) %>%
  group_by(site_id,latitude,longitude) %>% count() %>% select(-n)


# We add data site ID

data.site$study_id <- "Ruan_Veldtman_Malus_domestica_South_Africa_2011"
data.site$crop <- "Malus domestica"
data.site$variety <- "Royal Gala"
data.site$management <- "IPM"
data.site$country <- "South Africa"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_year <- 2011
data.site$field_size <- NA
data.site$yield <- NA
data.site$yield_units <- NA
data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA
data.site$Publication <- "10.1038/ncomms8414"
data.site$Credit <- "J.F. Colville, R. Veldtman"
data.site$Email_contact <- "veldtman@sun.ac.za"

# Add sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)

for (i in sites){

  data.site$sampling_start_month[data.site$site_id==i] <-
    data_raw %>% filter(site_id==i) %>%
    select(month_of_study) %>% min()

  data.site$sampling_end_month[data.site$site_id==i] <-
    data_raw %>% filter(site_id==i) %>%
    select(month_of_study) %>% max()
}


###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>% select(site_id,Xylocopa.sp.,Honeybees) %>%
  gather("Organism_ID","abundance",c(Xylocopa.sp.,Honeybees)) %>% filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Honeybees"] <- "honeybees"
list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"

#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)


insect_sampling <- tibble(
  study_id = "Ruan_Veldtman_Malus_domestica_South_Africa_2011",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = 2*(8-1)*5,
  total_sampled_time = 2*8*5, #netting+observations
  total_sampled_flowers = 2*8*758.5*0.5, #only one side of each tree is scanned
  Description = "On each survey, one side of eight trees along a transect (trees spaced 5 m apart) were scanned for five minutes each and bee species were recorded and voucher specimens collected (average of 758.5 +/- 265.1 [1SD] open flowers per apple tree)."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Ruan_Veldtman_Malus_domestica_South_Africa_2011.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "honeybees" "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,bumblebees=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,other_flies=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
  select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

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
percentage_species_morphos <- 0.9

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="only bees")

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
  total_sampled_area = 70,
  total_sampled_time = 80,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$total*60*100/80/(2*8*758.5*0.5),
  visit_honeybee = data.site$honeybees*60*100/80/(2*8*758.5*0.5),
  visit_bombus = data.site$bumblebees*60*100/80/(2*8*758.5*0.5),
  visit_wildbees = data.site$other_wild_bees*60*100/80/(2*8*758.5*0.5),
  visit_syrphids = data.site$syrphids*60*100/80/(2*8*758.5*0.5),
  visit_humbleflies = data.site$humbleflies*60*100/80/(2*8*758.5*0.5),
  visit_other_flies = data.site$other_flies*60*100/80/(2*8*758.5*0.5),
  visit_beetles = data.site$beetles*60*100/80/(2*8*758.5*0.5),
  visit_lepidoptera = data.site$lepidoptera*60*100/80/(2*8*758.5*0.5),
  visit_nonbee_hymenoptera = data.site$non_bee_hymenoptera*60*100/80/(2*8*758.5*0.5),
  visit_others = data.site$other*60*100/80/(2*8*758.5*0.5),
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Ruan_Veldtman_Malus_domestica_South_Africa_2011.csv")
#setwd(dir_ini)
