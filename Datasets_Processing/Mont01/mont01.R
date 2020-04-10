
library(tidyverse)
library(openxlsx)

dir_ini <- getwd()

##########################
#Data: Ana Montero Castaño: mont01

# Supplemental information: In some of the fields, varieties are grown together and farmers 
# do not know which rows have each variety (I worked with two varieties that 
# morphologically and phenologically are difficult to distinguish). In the ones that I 
# had that information, I have included one row per variety, but notice that the information
# about the pollinator community is duplicated. That is, I conducted pollinator censuses in 
# the fields but did not noted whether I was observing one variety or another

##########################


data.site <- read.xlsx("Copy of Crop_pollination_database_AnaMontero.xlsx",
                          sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

data.site$study_id <- "mont01"

data.site %>% group_by(site_id) %>% count()

# Update names of duplicated sites:

data.site$site_id[data.site$site_id %in% c("PLE","SHE","WAL")] <- 
  paste(data.site$site_id[data.site$site_id %in% c("PLE","SHE","WAL")],
        data.site$variety[data.site$site_id %in% c("PLE","SHE","WAL")],sep = "_") 



###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id=data.site$study_id,
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=NA,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  pollinator_richness=data.site$pollinator_richness,
  richness_estimator_method=data.site$richness_estimator_method,
  abundance=data.site$total,
  ab_honeybee=data.site$honeybees,
  ab_bombus=data.site$bumblebees,
  ab_wildbees=data.site$other_wild_bees,
  ab_syrphids=data.site$syrphids,
  ab_humbleflies=data.site$humbleflies,
  ab_other_flies=data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others=data.site$other,
  total_sampled_area=NA,
  total_sampled_time=NA,
  visitation_rate_units=NA,
  visitation_rate=NA,
  visit_honeybee=NA,
  visit_bombus=NA,
  visit_wildbees=NA,
  visit_syrphids=NA,
  visit_humbleflies=NA,
  visit_other_flies=NA,
  visit_beetles=NA,
  visit_lepidoptera=NA,
  visit_nonbee_hymenoptera=NA,
  visit_others=NA,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_mont01.csv")
setwd(dir_ini)
