
# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(sp)
library(maps)
library(maptools)
library(readODS)

dir_ini <- getwd()

# Load data
dir <- "Processing_files/Datasets_Processing/Diego_Nabaes_Jodar_Apple_Argentina_2019/"
file <- paste0(dir,"Diego_Nabaes_Jodar_Apple_Argentina_2019.ods")

data.site <- read_ods(file, sheet = "field_level_data") %>% as_tibble()
insect_sampling <- read_ods(file, sheet = "insect_sampling") %>% as_tibble()
data.ownership <- read_ods(file, sheet = "ownership") %>% as_tibble()

# Country

###############################################################################################
# FUNCTION: latlong2country
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
# SOURCE: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
###############################################################################################

latlong2country <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('world', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)

  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

geo_data <- data.site %>%
  select(x=longitude,y=latitude)

# Sanity check
data.site$country == latlong2country(geo_data[,1:2])

# Add crop
data.site$crop <- "Malus domestica"

# Fix management
data.site$management %>% unique()

data.site$management[data.site$management=="Other Conventional Practices"] <- "conventional"
data.site$management[data.site$management=="Organic Agriculture"] <- "organic"

# Addapt yield_units
data.site$yield_units[data.site$yield_units=="Fruit set (%)"] <- "fruit set (%)"

###########################
# SAMPLING DATA
###########################


insect_sampling_file <- data.frame(
  study_id  = insect_sampling$study_id,
  site_id  = insect_sampling$site_id,
  sampling_method  = insect_sampling$sampling_method,
  pollinator  = insect_sampling$pollinator,
  identified_to = insect_sampling$identified_to,
  guild  = insect_sampling$guild,
  abundance  = insect_sampling$abundance,
  total_sampled_area = insect_sampling$total_sampled_area,
  total_sampled_time  = insect_sampling$total_sampled_time,
  total_sampled_flowers = insect_sampling$total_sampled_flowers,
  description = insect_sampling$description,
  notes = NA)

# Save new insect sampling templates

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_file, paste0(dir, "Diego_Nabaes_Jodar_Apple_Argentina_2019.csv"))
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- insect_sampling_file %>% filter(!is.na(guild)) %>%
  select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(humbleflies=0,beetles=0,
                                  other=0,
                                  total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])


data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))

#######################################
# VISITATION RATES
#######################################

# Add site observations

visit_aux <- insect_sampling_file %>% filter(!is.na(guild)) %>%
  select(study_id,site_id,guild,abundance,total_sampled_time,total_sampled_flowers) %>%
  mutate(visit_rate=abundance*60*100/total_sampled_time/total_sampled_flowers) %>%
  group_by(study_id,site_id,guild) %>% count(wt=visit_rate) %>%
  spread(key=guild, value=n)

names(visit_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(humbleflies=0,beetles=0,
                                          other=0,
                                          total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(3:ncol(visit_aux))])

visit_aux <- visit_aux %>% rename(visit_honeybee = honeybees,
                                  visit_bombus = bumblebees,
                                  visit_wildbees = other_wild_bees,
                                  visit_syrphids = syrphids,
                                  visit_humbleflies = humbleflies,
                                  visit_other_flies = other_flies,
                                  visit_beetles = beetles,
                                  visit_lepidoptera = lepidoptera,
                                  visit_nonbee_hymenoptera = non_bee_hymenoptera,
                                  visit_others = other,
                                  visitation_rate = total)

data.site <- data.site %>% left_join(visit_aux, by = c("study_id","site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling_file %>% filter(!is.na(guild)) %>%
  select(study_id, site_id,pollinator,abundance)%>%
  group_by(study_id,site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>%
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,3:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator
}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",
         richness_restriction="all visitors considered"
)

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))

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
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=data.site$zone_UTM,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field_size,
  yield = data.site$yield,
  yield_units = data.site$yield_units,
  yield2 = data.site$yield2,
  yield2_units = data.site$yield2_units,
  yield_treatments_no_pollinators = data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement = data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2 = data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2 = data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant = data.site$fruits_per_plant,
  fruit_weight = data.site$fruit_weight,
  plant_density = data.site$plant_density,
  seeds_per_fruit = data.site$seeds_per_fruit,
  seeds_per_plant = data.site$seeds_per_plant,
  seed_weight = data.site$seed_weight,
  sampling_richness ="transects",
  observed_pollinator_richness = data.site$observed_pollinator_richness.y,
  other_pollinator_richness = data.site$other_pollinator_richness.y,
  other_richness_estimator_method = data.site$other_richness_estimator_method.y,
  richness_restriction = data.site$richness_restriction.y,
  sampling_abundance  ="transects",
  abundance = data.site$total,
  ab_honeybee = data.site$honeybees,
  ab_bombus = data.site$bumblebees,
  ab_wildbees = data.site$other_wild_bees,
  ab_syrphids = data.site$syrphids,
  ab_humbleflies= data.site$humbleflies,
  ab_other_flies= data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others = data.site$other,
  total_sampled_area = NA,
  total_sampled_time = 30,
  sampling_visitation = "transects",
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$visitation_rate.y,
  visit_honeybee = data.site$visit_honeybee.y,
  visit_bombus = data.site$visit_bombus.y,
  visit_wildbees = data.site$visit_wildbees.y,
  visit_syrphids = data.site$visit_syrphids.y,
  visit_humbleflies = data.site$visit_humbleflies.y,
  visit_other_flies = data.site$visit_other_flies.y,
  visit_beetles = data.site$visit_beetles.y,
  visit_lepidoptera = data.site$visit_lepidoptera.y,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera.y,
  visit_others = data.site$visit_others.y,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, paste0(dir,"field_level_data_Diego_Nabaes_Jodar_Apple_Argentina_2019.csv"))
#setwd(dir_ini)
