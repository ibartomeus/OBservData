
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
#library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 1_RiccardoBommarco_RedClover_2008
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/1_5_RiccardoBommarco_RedClover/Clover_studydata_STEP_WP4.xls",
                       sheet = "DATAWP4STUDY")

data_raw <- as_tibble(data_raw)

# Select the results for the corresponding study year

data_raw <- data_raw %>% filter(Year==2008)

# Fix species names

tax_ID <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/1_5_RiccardoBommarco_RedClover/Clover_studydata_STEP_WP4.xls",
                       sheet = "species")

tax_ID$Organism_ID <- NA
tax_ID$Organism_ID <- paste0(tax_ID$Genus," ",tax_ID$Species)
tax_ID$Organism_ID[tax_ID$Organism_ID=="NA NA"] <- "unidentified solitary bee"

colnames(data_raw) <- c(c("sampling_year","site_id","crop","N sites"),tax_ID$Organism_ID)


# Fix region name

region_list <- c("M" = "Ostergotland","E"="Skane")
data_raw$site_id <- unname(region_list[as.character(data_raw$site_id)])



##############
# Data site
##############

data.site <- data_raw %>% select(sampling_year,site_id,crop)

# We add data site ID

data.site$study_id <- "Riccardo_Bommarco_Trifolium_pratense_Sweden_2008"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Sweden"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <- 6
data.site$sampling_end_month <- 7
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
data.site$Publication <- "10.1098/rspb.2011.0647"
data.site$Credit <- "Riccardo Bommarco, Ola Lundin, Henrik G. Smith, and Maj Rundlof"
data.site$Email_contact <- "riccardo.bommarco@slu.se"

data.site$latitude <- NA
data.site$longitude <- NA

###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw[,c(2,4:ncol(data_raw))]

# Rearrange data
full_name <- colnames(data_raw_obs)
data_raw_obs <- data_raw_obs %>% gather("Organism_ID",
                                        "abundance",
                                        full_name[3:length(full_name)]) %>%
  filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()


#  Psithyrus rupestris, Psithyrus vestalis <- bumblebees
list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "bumblebees"

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
  study_id = "Riccardo_Bommarco_Trifolium_pratense_Sweden_2008",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "transects",
  abundance = data_obs_guild$abundance,
  total_sampled_area = 2*50*data_obs_guild$'N sites', # In 2008, each site was visited twice
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "Bumble-bees were collected along 1 m wide and 50 m long transects in 14 sites. Each site was visited twice."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Riccardo_Bommarco_Trifolium_pratense_Sweden_2008.csv")
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

# There are "bumblebees" "honeybees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,other_wild_bees=0,
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

#################
#SAMPLING DATA

data_sampling <- insect_sampling %>% group_by(site_id,total_sampled_area,total_sampled_time) %>% count() %>%
  select(-n)

data.site <- data.site %>% left_join(data_sampling, by = "site_id")

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
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
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
#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Riccardo_Bommarco_Trifolium_pratense_Sweden_2008.csv")
#setwd(dir_ini)

