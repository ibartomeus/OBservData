# load libraries
library(tidyverse)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Processing_files/Datasets_Processing/Alison_D_Oreilly_Brassica_napus_Ireland_2019/Alison O'Reilly_Crop_pollination_database .xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)


# New study ID
data.site$study_id <- "Alison_D_Oreilly_Brassica_napus_Ireland_2019"

# Fix, crop, yield_units

data.site$crop <- "Brassica napus"
data.site$country <- "Ireland"
data.site <- data.site %>%
  separate(field_size,c("field_size","unit_aux"),"ha") %>%
  select(-unit_aux)

data.site$yield_units <- "tonne/acre"

data.site$email <- "alison.oreilly@ucdconnect.ie/dara.stanley@ucd.ie"
data.site$visitation_rate_units <- "flowers visited per hour" #"flowers_visited/min"

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
  visitation_rate_units = data.site$visitation_rate_units,
  visitation_rate = 60*data.site$visitation_rate,
  visit_honeybee = 60*data.site$visit_honeybee,
  visit_bombus = 60*data.site$visit_bombus,
  visit_wildbees = 60*data.site$visit_wildbees,
  visit_syrphids = 60*data.site$visit_syrphids,
  visit_humbleflies = 60*data.site$visit_humbleflies,
  visit_other_flies = 60*data.site$visit_other_flies,
  visit_beetles = 60*data.site$visit_beetles,
  visit_lepidoptera = 60*data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = 60*data.site$visit_nonbee_hymenoptera,
  visit_others = 60*data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Alison_D_Oreilly_Brassica_napus_Ireland_2019.csv")
#setwd(dir_ini)
