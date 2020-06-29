# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Crop_pollination_database_Almond_Mallorca.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

# Remove last row

data.site <- data.site[1:35,]

data.site %>% group_by(site_id,sampling_year) %>% count()

# New study ID
data.site$study_id <- "Amparo_Lázaro_Prunus_dulcis_several_years"

# Fix unknow variety
data.site$variety[data.site$variety=="Unknown"] <- NA

# Rename management
x <- data.site %>% select(site_id,sampling_year,management,ab_honeybee)
x
data.site$management <- NA

#Publication

data.site$Publication <- "10.1016/j.agee.2018.05.004; 10.1016/j.agee.2019.02.009"


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
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  richness_restriction = data.site$richness_restriction,
  abundance = data.site$abundance,
  ab_honeybee = data.site$ab_honeybee,
  ab_bombus = data.site$ab_bombus,
  ab_wildbees = data.site$ab_wildbees,
  ab_syrphids = data.site$ab_syrphids,
  ab_humbleflies= data.site$ab_humbleflies,
  ab_other_flies= data.site$ab_other_flies,
  ab_beetles=data.site$ab_beetles,
  ab_lepidoptera=data.site$ab_lepidoptera,
  ab_nonbee_hymenoptera=data.site$ab_nonbee_hymenoptera,
  ab_others = data.site$ab_others,
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  visitation_rate_units = "visits per 100 flowers and hour",
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

field_level_data_2015 <- field_level_data %>% filter(sampling_year==2015)
field_level_data_2016 <- field_level_data %>% filter(sampling_year==2016)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2015, "field_level_data_Amparo_Lázaro_Prunus_dulcis_2015.csv")
write_csv(field_level_data_2016, "field_level_data_Amparo_Lázaro_Prunus_dulcis_2016.csv")
setwd(dir_ini)

# NOTES ABOUT HONEYBEE MANAGEMENT:
# 
# SesCabanasses	2015	Honeybee hives	686
# SonCos	2015	Honeybee hives	962
# SonMarrano	2015	Honeybee hives	664
# SonPujol	2015	Honeybee hives	1241
# Xorrigo	2015	Honeybee hives	1146
# SesCabanasses	2016	Honeybee hives	554
# SonCos	2016	Honeybee hives	529
# SonMarrano	2016	Honeybee hives	572
# SonPujol	2016	Honeybee hives	932
# Xorrigo	2016	Honeybee hives	448