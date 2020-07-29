# load libraries
library(tidyverse)
library("iNEXT")
#library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")

# Malus_domestia_Norway_2013

# Load data
data_raw <- read.xlsx("Norway/Norway_JÅ.xlsx",startRow = 1)

# Select data 
data_raw <- data_raw %>% filter(Crop_species!="Trifolium pratense")



###########################
# SAMPLING DATA
###########################

# NOTE: RICHNESS IN GARIBALDI'S TEMPLATE IS NOT OK. THE CORRECT ONE IS THAT IN
# SCIENCE SUPPL. MAT.

# TRANSECTS ARE USED TO MEASURE RICHNESS AND HONEYBEES ARE NOT INCLUDED

data_raw_obs_ini2 <- read.xlsx("Norway/Apple_Norway_V2_lucas.xlsx",startRow = 3)

which(names(data_raw_obs_ini2)=="Bombus_lucorum")
which(names(data_raw_obs_ini2)=="Syrphidae_sp.")

transect_counts <- data_raw_obs_ini2[,c(3,41,42,43,54:90)]

transect_counts$Syrphidae_sp.[transect_counts$Syrphidae_sp.=="\"Many\""] <- NA
transect_counts$Syrphidae_sp. <- as.numeric(transect_counts$Syrphidae_sp.)

transect_counts2 <- transect_counts %>% unique() %>% select(-Transect,-Date,-Time) #%>%
  #group_by(Site_name) %>% summarise_all(sum)

transect_counts3 <- transect_counts2 %>% gather(Organism_ID, abundance, c(2:38)) %>%
  filter(abundance>0)

transect_counts3$Guild <- NA

transect_counts3$Organism_ID %>% unique()

transect_counts3$Guild[grepl("Bombus",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
transect_counts3$Guild[grepl("Chrysotoxum",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Epistrophe",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Parasyrphus",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Syrphus",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Dasysyrphus",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Platycheirus",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Eristalis",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Leucozona.lucorum",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Empididae.sp.",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "other_flies"

transect_counts3$Guild[grepl("Solitary_bee",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
transect_counts3$Guild[grepl("Vespidae",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "non_bee_hymenoptera"
transect_counts3$Guild[grepl("Syrphidae",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Melanostoma",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Rhingia",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Meliscaeva_cinctella",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Orthonevra_geniculata",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "syrphids"
transect_counts3$Guild[grepl("Melanogaster_sp.",transect_counts3$Organism_ID,ignore.case = FALSE)] <- "other_flies"


transect_counts3 %>% filter(is.na(Guild)) %>% pull(Organism_ID)


#######################
# INSECT SAMPLING TRANSECTS
#######################

# Remove entries with zero abundance
transect_counts3  <- transect_counts3  %>% filter(abundance>0)

insect_sampling_transects <- tibble(
  study_id = "Jens_Åström_Malus_domestica_Norway_2013",
  site_id = transect_counts3$Site_name,
  pollinator = transect_counts3$Organism_ID,
  guild = transect_counts3$Guild,
  sampling_method = "sweep net",
  abundance = transect_counts3$abundance,
  total_sampled_area = 50*6*4,
  total_sampled_time = 5*6*4,
  total_sampled_flowers = NA,
  Description = "In each field abundance and richness of floral visitors was analysed by sweep-netting all visitors along six 25 m long and 2 m wide transects for five minutes each, for a total of 30 minutes of sampling. Sampling was further repeated on at least four dates during the main flowering period. Visitation rates were measured by observing a fixed number of open flowers and counting the number of those that are visited by a pollinator.")


###########################
# OBSERVATIONS
###########################

###########################
# VISITATION RATE DATA
###########################

# NOTE: RICHNESS IN GARIBALDI'S TEMPLATE IS NOT OK. THE CORRECT ONE IS THAT IN
# SCIENCE SUPPL. MAT.


which(names(data_raw_obs_ini2)=="Apis_mellifera")
which(names(data_raw_obs_ini2)=="Other_spp.")

data_raw_obs2 <- data_raw_obs_ini2[,c(3,41,42,43,44:50)]

data_raw_obs2 %>% group_by(Site_name,Transect) %>% count() #Some places appears twice
data_raw_obs2 %>% unique() %>% group_by(Site_name,Transect) %>% count()

visitation <- data_raw_obs2 %>% unique() %>% select(-Transect,-Date,-Time) 

visitation2 <- visitation %>% gather(Organism_ID, abundance, c(2:8)) %>%
  filter(abundance>0)

visitation2$Guild <- NA

visitation2$Organism_ID %>% unique()

visitation2$Guild[grepl("Bombus",visitation2$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
visitation2$Guild[grepl("Syrphidae",visitation2$Organism_ID,ignore.case = FALSE)] <- "syrphids"
visitation2$Guild[grepl("Apis_mell",visitation2$Organism_ID,ignore.case = FALSE)] <- "honeybees"
visitation2$Guild[grepl("Other_Diptera",visitation2$Organism_ID,ignore.case = FALSE)] <- "other_flies"
visitation2$Guild[grepl("Vespidae_spp.",visitation2$Organism_ID,ignore.case = FALSE)] <- "non_bee_hymenoptera"
visitation2$Guild[grepl("Other_spp",visitation2$Organism_ID,ignore.case = FALSE)] <- "unknown"



visitation2 %>% filter(is.na(Guild)) %>% pull(Organism_ID)


#######################
# INSECT SAMPLING OBSERVATIONS
#######################

# Remove entries with zero abundance
visitation2  <- visitation2  %>% filter(abundance>0)

insect_sampling_observations <- tibble(
  study_id = "Jens_Åström_Malus_domestica_Norway_2013",
  site_id = visitation2$Site_name,
  pollinator = visitation2$Organism_ID,
  guild = visitation2$Guild,
  sampling_method = "observations",
  abundance = visitation2$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = 250*6*4,
  Description = "In each field abundance and richness of floral visitors was analysed by sweep-netting all visitors along six 25 m long and 2 m wide transects for five minutes each, for a total of 30 minutes of sampling. Sampling was further repeated on at least four dates during the main flowering period. Visitation rates were measured by observing a fixed number of open flowers and counting the number of those that are visited by a pollinator.")

insect_sampling <- bind_rows(insect_sampling_transects,insect_sampling_observations)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Jens_Åström_Malus_domestica_Norway_2013.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  transect_counts3 %>%
  filter(!is.na(abundance)) %>%
  group_by(Site_name,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  filter(!is.na(Guild)) %>%
  group_by(Site_name,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "bumblebees"  "other_flies" "syrphids"   

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(honeybees=0,humbleflies=0,
                                          beetles=0,
                                          lepidoptera=0, other =0,
                                          humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

abundance_aux$honeybees <- NA #Honeybees are not included in the transect counts

data_raw <- data_raw %>% left_join(abundance_aux,by="Site_name")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- transect_counts3 %>% 
  filter(!is.na(Guild)) %>%
  select(Site_name,Organism_ID,abundance)%>%
  group_by(Site_name,Organism_ID) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>% 
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

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

richness_aux <- abundace_field %>% select(Site_name,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="Honeybees are not included in richness metrics")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data_raw <- data_raw %>% left_join(richness_aux,by="Site_name")

###########################
# VISITATION RATE DATA
###########################

# NOTE: RICHNESS IN GARIBALDI'S TEMPLATE IS NOT OK. THE CORRECT ONE IS THAT IN
# SCIENCE SUPPL. MAT.

data_raw_obs_ini2 <- read.xlsx("Norway/Apple_Norway_V2_lucas.xlsx",startRow = 3)

which(names(data_raw_obs_ini2)=="Apis_mellifera")
which(names(data_raw_obs_ini2)=="Other_spp.")

data_raw_obs2 <- data_raw_obs_ini2[,c(3,41,42,43,44:50)]

data_raw_obs2 %>% group_by(Site_name,Transect) %>% count() #Some places appears twice
data_raw_obs2 %>% unique() %>% group_by(Site_name,Transect) %>% count()

data_raw_obs2 %>% unique() %>% group_by(Site_name) %>% count()

mean_visitation <- data_raw_obs2 %>% unique() %>% select(-Transect,-Date,-Time) %>% 
  group_by(Site_name) %>%
  summarize_all(mean,na.rm=T)

mean_visitation$total <- rowSums(mean_visitation[,c(2:ncol(mean_visitation))])

data_raw <- data_raw %>% left_join(mean_visitation,by="Site_name")
###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = "Jens_Åström_Malus_domestica_Norway_2013",
  site_id = data_raw$Site_name,
  crop = data_raw$Crop_species,
  variety = data_raw$Crop_variety,
  management = NA,
  country = data_raw$Country,
  latitude = data_raw$Latitude_decimal.degrees,
  longitude = data_raw$Longitude_decimal.degrees,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = data_raw$Year,
  field_size = data_raw$Field_size_ha,
  yield=data_raw$SiteMean_Yield,
  yield_units=NA,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight= NA,
  plant_density=10000*data_raw$Number_plants_ha,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=data_raw$observed_pollinator_richness,
  other_pollinator_richness=data_raw$other_pollinator_richness,
  other_richness_estimator_method=data_raw$other_pollinator_richness,
  richness_restriction = data_raw$richness_restriction,
  abundance = data_raw$total.x,
  ab_honeybee = data_raw$honeybees,
  ab_bombus = data_raw$bumblebees,
  ab_wildbees = data_raw$other_wild_bees,
  ab_syrphids = data_raw$syrphids,
  ab_humbleflies= data_raw$humbleflies,
  ab_other_flies= data_raw$other_flies,
  ab_beetles=data_raw$beetles,
  ab_lepidoptera=data_raw$lepidoptera,
  ab_nonbee_hymenoptera=data_raw$non_bee_hymenoptera,
  ab_others = data_raw$other,
  total_sampled_area = 50*6*4,
  total_sampled_time = 5*6*4,
  visitation_rate_units = "visits per 100 flowers",
  visitation_rate = data_raw$total.y/2.5,
  visit_honeybee = data_raw$Apis_mellifera/2.5,
  visit_bombus = data_raw$Bombus_spp./2.5,
  visit_wildbees = data_raw$Solitary_bee_spp./2.5,
  visit_syrphids = data_raw$Syrphidae_spp./2.5,
  visit_humbleflies = NA,
  visit_other_flies = data_raw$Other_Diptera_spp./2.5,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = "10.1126/science.aac7287",
  Credit = "Jens Åström",
  Email_contact = "jens.astrom@nina.no"
)


# Adding JEN's comments

field_level_data_mod <- read.delim("Norway/field_level_data_Jens_Åström_Malus_domestica_Norway_2013_JÅ.csv",sep = ";")

field_level_data$sampling_start_month <- field_level_data_mod$sampling_start_month
field_level_data$sampling_end_month <- field_level_data_mod$sampling_end_month
field_level_data$management <- "conventional"
field_level_data$longitude <- field_level_data_mod$longitude
field_level_data$latitude <- field_level_data_mod$latitude


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Jens_Åström_Malus_domestica_Norway_2013.csv")
setwd(dir_ini)
