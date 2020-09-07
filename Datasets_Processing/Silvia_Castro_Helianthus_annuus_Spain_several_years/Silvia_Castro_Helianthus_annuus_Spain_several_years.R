
library(tidyverse)
library(openxlsx)
library(parzer) #Transforming latitude and longitude
library(stringr)
library(iNEXT)
###################################
#Data holder: Silvia Castro
###################################

dir_ini <- getwd()
options(digits=14)

data.site <- read.xlsx("Crop_pollination_database_SUNFLOWER_2020-07-29_corrected.xlsx",
                          sheet = "field_level_data", startRow = 1)

data.site <- as_tibble(data.site)

# Check site_id
data.site %>% group_by(site_id,sampling_year) %>% count() %>% filter(n>1)

# Change study IDs

data.site$study_id[data.site$sampling_year==2017] <- "Silvia_Castro_Helianthus_annuus_Spain_2017"
data.site$study_id[data.site$sampling_year==2018] <- "Silvia_Castro_Helianthus_annuus_Spain_2018"

# Fix crop

data.site$crop <- "Helianthus annuus"

# Fix management

data.site$management <- "conventional"

# Convert Latitude/Longitude from degrees min sec to decimal

data.site$latitude <- parse_lat(data.site$latitude)

# when parsing longitude, some NAs appears
which(is.na(parse_lon(data.site$longitude)))
data.site$longitude[which(is.na(parse_lon(data.site$longitude)))]

data.site$longitude[2] <- "-3.56233ºW"
data.site$longitude[15] <- "-3.10787ºW"
data.site$longitude[20] <- "-3.61491ºW"
data.site$longitude[21] <- "-3.56197ºW"
data.site$longitude[26] <- "-4.24806ºW"
data.site$longitude[33] <- "-3.11277ºW"
data.site$longitude <- parse_lon(data.site$longitude)

# Fix yield units

data.site$yield_units <- "kg/ha"

###########################
# SAMPLING DATA
###########################

insect_sampling <- read.xlsx("Crop_pollination_database_SUNFLOWER_2020-07-29_corrected.xlsx", sheet = "insect_sampling")

#Sanity check site_id

data.site$site_id[!data.site$site_id %in% insect_sampling$site_id]
insect_sampling$site_id[!insect_sampling$site_id %in% data.site$site_id]

# New study ID

field_ID_2017 <- data.site %>% filter(sampling_year==2017) %>% select(site_id) %>% pull()
field_ID_2018 <- data.site %>% filter(sampling_year==2018) %>% select(site_id) %>% pull()

insect_sampling$study_id[insect_sampling$site_id %in% field_ID_2017] <- "Silvia_Castro_Helianthus_annuus_Spain_2017"
insect_sampling$study_id[insect_sampling$site_id %in% field_ID_2018] <- "Silvia_Castro_Helianthus_annuus_Spain_2018"

# Sanity check
which(insect_sampling$study_id=="POLLOLE project_Burgos")


#Fix guilds

insect_sampling %>% group_by(guild) %>% count()
insect_sampling$guild[insect_sampling$guild=="nonbee_hymenoptera"] <- "non_bee_hymenoptera"
insect_sampling$guild[insect_sampling$guild=="wildbees"] <- "other_wild_bees"
insect_sampling$guild[insect_sampling$guild=="bombus"] <- "bumblebees"
insect_sampling$guild[insect_sampling$guild=="honeybee"] <- "honeybees"
insect_sampling$guild[insect_sampling$guild=="Lepidoptera"] <- "lepidoptera"
insect_sampling$guild[insect_sampling$guild=="others "] <- "others"
insect_sampling %>% group_by(guild) %>% count()

# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description[insect_sampling$sampling_method=="census"] <- insect_sampling$Column1[insect_sampling$sampling_method=="census"]

# Modify pan trap sampling area + total sampling flowers

insect_sampling$total_sampled_area[insect_sampling$sampling_method=="pan-traps"] <- NA
insect_sampling$total_sampled_flowers[insect_sampling$sampling_method=="pan-traps"] <- NA

# Save new insect sampling templates

insect_sampling_2017 <- insect_sampling %>% 
  filter(study_id=="Silvia_Castro_Helianthus_annuus_Spain_2017",
         !is.na(abundance), abundance>0) %>% select(-Column1)

insect_sampling_2018 <- insect_sampling %>% 
  filter(study_id=="Silvia_Castro_Helianthus_annuus_Spain_2018",
         !is.na(abundance), abundance>0) %>% select(-Column1)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_2017, "insect_sampling_Silvia_Castro_Helianthus_annuus_Spain_2017.csv")
write_csv(insect_sampling_2018, "insect_sampling_Silvia_Castro_Helianthus_annuus_Spain_2018.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- insect_sampling %>% filter(!is.na(abundance),sampling_method=="census") %>%
  select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>% 
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])


data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling %>% filter(!is.na(abundance),sampling_method=="census") %>%
  select(study_id, site_id,pollinator,abundance)%>%
  group_by(study_id,site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>% 
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,3:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 0.5

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))


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
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$mean_fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness.y,
  other_pollinator_richness=data.site$other_pollinator_richness.y,
  other_richness_estimator_method=data.site$other_richness_estimator_method.y,
  richness_restriction = data.site$richness_restriction.y,
  abundance = data.site$total,#data.site$abundance,
  ab_honeybee = data.site$honeybees,#data.site$ab_honeybee,
  ab_bombus = data.site$bumblebees,#data.site$ab_bombus,
  ab_wildbees = data.site$other_wild_bees,#data.site$ab_wildbees,
  ab_syrphids = data.site$syrphids,#data.site$ab_syrphids,
  ab_humbleflies= data.site$humbleflies,#data.site$ab_humbleflies,
  ab_other_flies= data.site$other_flies,#data.site$ab_other_flies,
  ab_beetles= data.site$beetles,#data.site$ab_beetles,
  ab_lepidoptera= data.site$lepidoptera,#data.site$ab_lepidoptera,
  ab_nonbee_hymenoptera= data.site$non_bee_hymenoptera,#data.site$ab_nonbee_hymenoptera,
  ab_others = data.site$others,#data.site$ab_others,
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  visitation_rate_units = data.site$visitation_rate_units,
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = data.site$visit_honeybee,
  visit_bombus = data.site$visit_bombus,
  visit_wildbees = data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)


field_level_data_2017 <- field_level_data %>% filter(sampling_year==2017) 
field_level_data_2018 <- field_level_data %>% filter(sampling_year==2018) 


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2017, "field_level_data_Silvia_Castro_Helianthus_annuus_Spain_2017.csv")
write_csv(field_level_data_2018, "field_level_data_Silvia_Castro_Helianthus_annuus_Spain_2018.csv")
setwd(dir_ini)

