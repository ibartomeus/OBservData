
library(tidyverse)
library(openxlsx)
library(parzer) #Transforming latitude and longitude
library(stringr)

###################################
#Data holder: Violeta Hevia, hevi01 
###################################

dir_ini <- getwd()
options(digits=14)

datafield <- read.xlsx("Crop_pollination_database_PollOleGI - Burgos_v2.xlsx",
                          sheet = "field_level_data", startRow = 1)

datafield <- as_tibble(datafield)

# Check site_id
datafield %>% group_by(site_id,sampling_year) %>% count()

names(datafield)

datafield <- datafield %>%
  rename(field_size = field.size,
         fruits_per_plant = mean_fruits_per_plant,
         richness_estimator_method = `richness_estimator_&#10;Method`)


datafield$management <- "conventional"
datafield$crop <- "Helianthus annuus"
datafield$study_id <- "Silvia_Castro_Helianthus_annuus_Spain_several_years"

# Convert Latitude/Longitude from degrees min sec to decimal

datafield$latitude <- parse_lat(datafield$latitude)
datafield$longitude <- str_replace_all(datafield$longitude,pattern=" ", repl="")
datafield$longitude[2] <- "-3.56233ºW"
datafield$longitude[15] <- "-3.10787ºW"
datafield$longitude[20] <- "-3.61491ºW"
datafield$longitude[21] <- "-3.56197ºW"
datafield$longitude[26] <- "-4.24806ºW"
datafield$longitude[33] <- "-3.11277ºW"
datafield$longitude <- parse_lon(datafield$longitude)

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
  fruits_per_plant=datafield$fruits_per_plant,
  fruit_weight= datafield$fruit_weight,
  plant_density=datafield$plant_density,
  seeds_per_fruit=datafield$seeds_per_fruit,
  seeds_per_plant=datafield$seeds_per_plant,
  seed_weight=datafield$seed_weight,
  observed_pollinator_richness=NA,
  other_pollinator_richness=datafield$pollinator_richness,
  other_richness_estimator_method=datafield$richness_estimator_method,
  richness_restriction = NA,
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
write_csv(field_level_data, "field_level_data_Silvia_Castro_Helianthus_annuus_Spain_several_years.csv")
setwd(dir_ini)

