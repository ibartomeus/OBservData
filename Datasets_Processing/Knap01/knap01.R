
library(tidyverse)
library(openxlsx)

############################
# Data: Jessica Knapp: knap01
# Here richness and abundance were not obtained from pan-traps data.
############################

dir_ini <- getwd()
options(digits=14)


datafield <- read.xlsx("Crop_pollination_database_JessicaKnapp.xlsx",
                          sheet = "field_level_data", startRow = 1)


datafield <- as_tibble(datafield)
datafield <- datafield %>%
  rename(field_size = field.size,total_yield = `total_yield(KG/HA)`,
         fruits_per_plant = mean_fruits_per_plant,
         richness_estimator_method = `richness_estimator_.Method`)

datafield$country <- "UK"
datafield$sampling_year <- 2016
datafield$richness_estimator_method <- "observed"
datafield$management <- "conventional"
datafield$total_sampled_area <- 600
datafield$ab_wildbees <- NA # Aquí no están los datos de pantrap
datafield$ab_syrphids <- NA # Aquí no están los datos de pantrap
datafield$study_id <- "knap01"
datafield$Publication <- "10.1016/j.baae.2018.09.003"


###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = datafield$study_id,
  site_id = datafield$site_id,
  crop = datafield$crop,
  variety = datafield$variety,
  management = datafield$management,
  country = datafield$country,
  latitude = datafield$latitude,
  longitude = datafield$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = datafield$sampling_start_month,
  sampling_end_month = datafield$sampling_end_month,
  sampling_year = datafield$sampling_year,
  field_size = datafield$field_size,
  yield=datafield$total_yield,
  yield_units="kg/ha",
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight= NA,
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=datafield$pollinator_richness,
  other_pollinator_richness=NA,
  other_richness_estimator_method=NA,
  abundance = datafield$abundance,
  ab_honeybee = datafield$ab_honeybee,
  ab_bombus = datafield$ab_bombus,
  ab_wildbees = datafield$ab_wildbees,
  ab_syrphids = datafield$ab_syrphids,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = datafield$total_sampled_area,
  total_sampled_time = datafield$total_sampled_time,
  visitation_rate_units = NA,
  visitation_rate = datafield$visitation_rate,
  visit_honeybee = datafield$visit_honeybee,
  visit_bombus = datafield$visit_bombus,
  visit_wildbees = datafield$visit_wildbees,
  visit_syrphids = datafield$visit_syrphids,
  visit_humbleflies = NA,
  visit_other_flies = NA,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = datafield$Publication,
  Credit = datafield$Credit,
  Email_contact = datafield$email
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_knap01.csv")
setwd(dir_ini)

