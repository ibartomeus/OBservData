# load libraries
library(tidyverse)
library("iNEXT")
#library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")


# Trifolium pratense_Norway_2013

# Load data
data_raw <- read.xlsx("Norway/Norway_JÅ.xlsx",startRow = 1)


# Select data 
data_raw <- data_raw %>% filter(Crop_species=="Trifolium pratense")


# NOTE: RICHNESS IN GARIBALDI'S TEMPLATE IS NOT OK. THE CORRECT ONE IS THAT IN
# SCIENCE SUPPL. MAT.

richness_science <- 
  science_raw$Flower_visitor_richness[science_raw$Crop_System %in% 
                                        c("Norway Trifolium pratense 2013",
                                          "Norway Trifolium pratense 2014")]


###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = "Trifolium pratense_Norway_2013",
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
  observed_pollinator_richness=richness_science,
  other_pollinator_richness=NA,
  other_richness_estimator_method=NA,
  richness_restriction = NA,
  abundance = data_raw$Mean_transect_AllVisitors,
  ab_honeybee = data_raw$Mean_transect_Apis,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = data_raw$Minutes_NetSampling,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = 60*data_raw$AllVisitors_100flowers/5,
  visit_honeybee = 60*data_raw$Mean_transect_Apis/5,
  visit_bombus = NA,
  visit_wildbees = NA,
  visit_syrphids = NA,
  visit_humbleflies = NA,
  visit_other_flies = NA,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = "10.1126/science.aac7287",
  Credit = "Jens Åström",
  Email_contact = "jens.astrom@nina.no"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Trifolium pratense_Norway_2013.csv")
setwd(dir_ini)

# NOTES
# We assumed that visit to 100 flower were measured for 5 minites in each field
# We assumed that apis variables refers to honeybees