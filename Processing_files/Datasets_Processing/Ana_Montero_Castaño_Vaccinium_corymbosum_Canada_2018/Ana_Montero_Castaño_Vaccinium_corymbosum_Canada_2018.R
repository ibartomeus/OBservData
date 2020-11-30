
library(tidyverse)
library(openxlsx)

dir_ini <- getwd()

##########################
#Data: Ana Montero Casta?o: mont01

# Supplemental information: In some of the fields, varieties are grown together and farmers
# do not know which rows have each variety (I worked with two varieties that
# morphologically and phenologically are difficult to distinguish). In the ones that I
# had that information, I have included one row per variety, but notice that the information
# about the pollinator community is duplicated. That is, I conducted pollinator censuses in
# the fields but did not noted whether I was observing one variety or another

##########################


data.site <- read.xlsx("Processing_files/Datasets_Processing/Ana_Montero_Castaño_Vaccinium_corymbosum_Canada_2018/Copy of Crop_pollination_database_AnaMontero_v2.xlsx",
                          sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

data.site$study_id <- "Ana_Montero_Castaño_Vaccinium_corymbosum_Canada_2018"

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
  variety=data.site$variety,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=data.site$sampling_start_month,
  sampling_end_month=data.site$sampling_end_month,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=data.site$mean_fruits_per_plant,
  fruit_weight=data.site$fruit_weight,
  plant_density=NA,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=NA,
  other_pollinator_richness=data.site$pollinator_richness,
  other_richness_estimator_method=data.site$richness_estimator_method,
  abundance=data.site$abundance,
  ab_honeybee=data.site$ab_honeybee,
  ab_bombus=data.site$ab_bombus,
  ab_wildbees=data.site$ab_wildbees,
  ab_syrphids=data.site$ab_syrphids,
  ab_humbleflies=data.site$ab_humbleflies,
  ab_other_flies=data.site$ab_other_flies,
  ab_beetles=data.site$ab_beetles,
  ab_lepidoptera=data.site$ab_lepidoptera,
  ab_nonbee_hymenoptera=data.site$ab_nonbee_hymenoptera,
  ab_others=data.site$ab_others,
  total_sampled_area=data.site$total_sampled_area,
  total_sampled_time=data.site$total_sampled_time,
  visitation_rate_units="visits per 100 flowers and hour",
  visitation_rate=data.site$visitation_rate,
  visit_honeybee=data.site$visit_honeybee,
  visit_bombus=data.site$visit_bombus,
  visit_wildbees=data.site$visit_wildbees,
  visit_syrphids=data.site$visit_syrphids,
  visit_humbleflies=data.site$visit_humbleflies,
  visit_other_flies=data.site$visit_other_flies,
  visit_beetles=data.site$visit_beetles,
  visit_lepidoptera=data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera=data.site$visit_nonbee_hymenoptera,
  visit_others=data.site$visit_others,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)
#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Ana_Montero_Castaño_Vaccinium_corymbosum_Canada_2018.csv")
#setwd(dir_ini)
