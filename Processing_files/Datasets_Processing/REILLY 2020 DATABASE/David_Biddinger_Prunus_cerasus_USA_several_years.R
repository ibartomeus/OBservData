# Transpose James Reilly csv's to OBServ format

# load libraries
library(tidyverse)
#library("iNEXT")
#library(parzer)
#library(readxl)
#library(openxlsx)

dir_ini <- getwd()

# Imput file: almond_ca_dataset

# Load data

# data.site_mi <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/tart_cherry_mi_dataset.csv")
data.site_pa <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/tart_cherry_pa_dataset.csv") #%>% rename(wild_bee=wild_bees)

data.site <- data.site_pa #bind_rows(data.site_mi,data.site_pa)

# New site_id

data.site$site_id <- paste0(data.site$crop,"_",
                            data.site$state,"_",
                            data.site$site,"_",
                            data.site$transect)

# Add latitude and longitude
coordinates <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/ICP_sites_with_latlong_for_observ.csv") %>%
  rename(site=site_id)

data.site$crop_id <- paste0(data.site$crop,"_",
                            data.site$state)


data.site <- data.site %>% left_join(coordinates,by=c("crop_id","site")) %>%
  rename(latitude=lat,longitude=long)



# New study_id
data.site$study_id <- paste0("David_Biddinger_Prunus_cerasus_USA_",data.site$year)


# Crop latin name
data.site$crop <- "Prunus cerasus"

##############################
# Transpose yield and functioning metrics
##############################

data.site$yield <- data.site$number_of_fruit_mean
data.site$yield_units <- "number of fruits"
data.site$yield2 <- data.site$number_fruit_per_flower_mean
data.site$yield2_units <- "number of fruits per_flower"


data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_pollen_supplement <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$mean_fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

##############################
# PREPARE VISITATION RATES
##############################
names(data.site)

data.site$other_wild_bees <- data.site$all_bees - data.site$honey_bee - data.site$bumble_bee # + data.site$blue_orchard_bee

# sanity check

data.site$all_bees==(data.site$other_wild_bees +
                       data.site$honey_bee +
                       data.site$bumble_bee)

# Estimating visitation rates

data.site$visitation_rate_units <- "visits per flower"

data.site <- data.site %>% rowwise() %>%
  mutate(vist_total = sum(all_bees,
                          syrphid,
                          wasp,
                          #other_insect,
                          na.rm = T)) %>%
  mutate(vist_total = vist_total/number_of_flowers_mean)


data.site$vist_honeybees <- data.site$honey_bee/data.site$number_of_flowers_mean
data.site$vist_bumblebees <- data.site$bumble_bee/data.site$number_of_flowers_mean
data.site$vist_other_wild_bees <- data.site$other_wild_bees/data.site$number_of_flowers_mean
data.site$vist_syrphids <- data.site$syrphid/data.site$number_of_flowers_mean
data.site$vist_humbleflies <- NA
data.site$vist_other_flies <- NA
data.site$vist_beetles <- NA
data.site$vist_lepidoptera <- NA
data.site$vist_non_bee_hymenoptera <- data.site$wasp/data.site$number_of_flowers_mean

# Comment raised by James about Prunus cerasus
# 1) you can put the "other insects" into the "visits_others" category.
# These were apparently not flies, beetles, or wasps.
data.site$vist_others <- NA

################################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = data.site$study_id,
  site_id = data.site$site_id,
  crop = data.site$crop,
  variety = NA,
  management = NA,
  country = "USA",
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = data.site$year,
  field_size = NA,
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
  observed_pollinator_richness=NA,
  other_pollinator_richness=NA,
  other_richness_estimator_method=NA,
  richness_restriction = NA,
  abundance = NA,
  ab_honeybee = NA,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles= NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = NA,
  visitation_rate_units = data.site$visitation_rate_units,
  visitation_rate = data.site$vist_total,
  visit_honeybee = data.site$vist_honeybees,
  visit_bombus = data.site$vist_bumblebees,
  visit_wildbees = data.site$vist_other_wild_bees,
  visit_syrphids = data.site$vist_syrphids,
  visit_humbleflies = data.site$vist_humbleflies,
  visit_other_flies = data.site$vist_other_flies,
  visit_beetles = data.site$vist_beetles,
  visit_lepidoptera = data.site$vist_lepidoptera,
  visit_nonbee_hymenoptera = data.site$vist_non_bee_hymenoptera,
  visit_others = data.site$vist_others,
  Publication = "10.1098/rspb.2020.0922",
  Credit = "David Biddinger and all the co-authors in the referred publication",
  Email_contact = "djb134@psu.edu/jreilly45@gmail.com/rwinfree@rutgers.edu/isaacsr@msu.edu"
)


field_level_data$study_id %>% unique()

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_David_Biddinger_Prunus_cerasus_USA_2013-14-15.csv")
#setwd(dir_ini)
