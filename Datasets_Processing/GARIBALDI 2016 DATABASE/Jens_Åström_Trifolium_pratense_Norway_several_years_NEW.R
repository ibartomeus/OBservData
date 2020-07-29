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
  study_id = "Jens_Åström_Trifolium_pratense_Norway_several_years",
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
  observed_pollinator_richness=richness_science, #Parece una riqueza media
  other_pollinator_richness=NA,
  other_richness_estimator_method=NA,
  richness_restriction = "only honey bees and bombus sp.",
  abundance = data_raw$Mean_transect_AllVisitors, #Abundancia media por campo
  ab_honeybee = data_raw$Mean_transect_Apis,
  ab_bombus = data_raw$Mean_transect_NonApis,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = NA, #30 min de transectos + cantidad desconocidad de observaciones
  visitation_rate_units = "visits per 100 flowers",
  visitation_rate = data_raw$Mean_transect_AllVisitors/4, #30min son los transectos, las abundancias se calculan conobservaciones
  visit_honeybee = data_raw$Mean_transect_Apis/4,
  visit_bombus = data_raw$Mean_transect_NonApis/4,
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

# Adding JEN's comments

field_level_data_mod <- read_csv2("Norway/field_level_data_Jens_Åström_Trifolium_pratense_Norway_several_years_JÅ.csv")

field_level_data$sampling_start_month <- field_level_data_mod$sampling_start_month
field_level_data$sampling_end_month <- field_level_data_mod$sampling_end_month
field_level_data$management <- "conventional"

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Jens_Åström_Trifolium_pratense_Norway_several_years.csv")
setwd(dir_ini)

# We followed the FAO pollination deficit protocol which has two components for pollinator registering. Pollinator diversity is
# measured by identifying all species (within limits), on fixed transect walks during a fixed time period (usually in total 30
#                                                                                                          minutes for each sampling event, 5 minutes x 6 transects.). The abundance is measured by observing a fixed number of
# open flowers and counting the number of those that are visited by a pollinator. We usually did this for 400 flowers, but
# increased the number of flowers in case of low visitation rates. The point is to not have zero visitation records due to
# counting to few flowers. These numbers where then rescaled to represent visitations per 100 observed flowers in Lucas
# dataset in the combined international analysis.