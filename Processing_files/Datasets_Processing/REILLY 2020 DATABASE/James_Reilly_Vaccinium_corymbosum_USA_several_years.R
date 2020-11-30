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

data.site_fl <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/blueberry_fl_dataset.csv")
data.site_mi_bc_or <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/blueberry_mi_bc_or_dataset.csv") #%>% rename(wild_bee=wild_bees)

# Fix error in mi

data.site_mi_bc_or$site[data.site_mi_bc_or$state=="mi" & data.site_mi_bc_or$site==14] <- 24

data.site <- bind_rows(data.site_fl,data.site_mi_bc_or)

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
data.site$study_id <- paste0("James_Reilly_Vaccinium_corymbosum_USA_",data.site$year)


# Crop latin name
data.site$crop <- "Vaccinium corymbosum"

##############################
# Transpose yield and functioning metrics
##############################

data.site$yield <- data.site$mean_berry_wt_open
data.site$yield_units <- "berry weight"
data.site$yield2 <- data.site$fruit_set_open
data.site$yield2_units <- "fruit set"


data.site$yield_treatments_no_pollinators <- data.site$mean_berry_wt_bagged
data.site$yield_treatments_pollen_supplement <- data.site$mean_berry_wt_bagged
data.site$yield_treatments_no_pollinators2 <- data.site$fruit_set_bagged
data.site$yield_treatments_pollen_supplement2 <- data.site$fruit_set_hand
data.site$mean_fruits_per_plant <- NA
data.site$fruit_weight <- data.site$mean_berry_wt_open
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

##############################
# PREPARE VISITATION RATES
##############################
names(data.site)

data.site$other_wild_bees <- round(data.site$all_bees - data.site$honey_bee - data.site$bumble_bee,5) # + data.site$blue_orchard_bee

# sanity check

round(data.site$all_bees,5)==round(data.site$other_wild_bees +
                       data.site$honey_bee +
                       data.site$bumble_bee,5) # Row 16 is false

# Checking Row 16
data.site$all_bees[16]
data.site$other_wild_bees[16] + data.site$honey_bee[16] + data.site$bumble_bee[16] #OK

# Estimating visitation rates

# there is no information about flowers

data.site$number_of_flowers_mean <- 1

data.site$visitation_rate_units <- "visits"

data.site <- data.site %>% rowwise() %>%
  mutate(vist_total = sum(all_bees,
                           syrphid,
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
data.site$vist_non_bee_hymenoptera <- NA
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
  Credit = "James Reilly and all the co-authors in the referred publication",
  Email_contact = "jreilly45@gmail.com/rwinfree@rutgers.edu/isaacsr@msu.edu"
)


field_level_data$study_id %>% unique()

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_James_Reilly_Vaccinium_corymbosum_USA_2013-14-15.csv")
#setwd(dir_ini)
