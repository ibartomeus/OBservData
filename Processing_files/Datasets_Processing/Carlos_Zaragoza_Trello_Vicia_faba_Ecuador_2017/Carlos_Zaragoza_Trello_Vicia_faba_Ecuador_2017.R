
# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(sp)
library(maps)
library(maptools)

dir_ini <- getwd()

# Load data
dir <- "Processing_files/Datasets_Processing/Carlos_Zaragoza_Trello_Vicia_faba_Ecuador_2017/"
file <- paste0(dir,"Vicia_faba_abundancia.csv")
data.raw <- read_csv(file)


# Extract lat-long
data.site <- data.raw %>% separate(COORDENADAS,c("latitude","longitude"),"' ") %>%
  select(Campo,latitude,longitude) %>% rename(site_id = Campo) %>%
  mutate(study_id="Carlos_Zaragoza_Trello_Vicia_faba_Ecuador_2017") %>% unique()

data.site$latitude <- str_replace_all(data.site$latitude, fixed(" "), "")
data.site$latitude <- paste0(data.site$latitude, "'")
data.site$longitude <- str_replace_all(data.site$longitude, fixed(" "), "")
data.site$latitude <- parse_lat(data.site$latitude)
data.site$longitude <- parse_lon(data.site$longitude)

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

data.site$country <- latlong2country(geo_data[,1:2])

# Add sampling year
data.site$sampling_year <- 2017

# Add crop
data.site$crop <- "Vicia faba"

data.site$sampling_start_month <- 7
data.site$sampling_end_month <- 8



# Rename management
data.site$management <- NA

###########################
# SAMPLING DATA
###########################

insect_sampling <- data.raw %>% rename(site_id = Campo,
                                       pollinator = Grupo,
                                       abundance = Abundancia)

# New study ID
insect_sampling$study_id <- "Carlos_Zaragoza_Trello_Vicia_faba_Ecuador_2017"

# Sampling method
insect_sampling$sampling_method <- "transects"


#identified_to
insect_sampling %>% select(pollinator) %>% unique()
poll_info <- data.frame(
  pollinator=c("Solitary_bees","bombus","diptera","honeybee",NA,"syrphids"),
  identified_to=c("superfamily","genus","order","genus","unkown","family"),
  guild=c("other_wild_bees","bumblebees","other_flies","honeybees",NA,"syrphids"))

insect_sampling <- insect_sampling %>% left_join(poll_info,by="pollinator")

# Description
insect_sampling$Description <- "To explore pollinator biodiversity we selected 15 Faba bean fields that were at the same phenological stage and adjacent to a managed grassland (or a natural grassland area when it was not feasible). We conducted pollinator surveys along 25 days (from early July to middle August) by walking during 15 minutes along each of two 150 x 1 m standardized transects per site. One transect was located at the field edge while the central transect was located at 25 m inside the field from the edge. In 5 fields these transects were adapted to the middle of the field as the field did not reach 50 meters. Transect walks were conducted between 8:00 and 16:30, low wind speeds and temperatures above 15 degrees C. Due Andean high altitude weather characteristics, during shorts periods of rain (minutes) or low clouds we stopped the survey and resumed it when weather allowed. Sites were surveyed thrice in random order. We surveyed all honeybees, bumblebees, solitary bees, hoverflies and other Diptera (e.g: Calliphoridae, Tachinidae, Sarcophagidae) that visited flowers inside Faba beans field and at the margin of the crop capturing the pollinators with a net. Handling time invested in capturing pollinators was discounted. In total, the sampling effort was 90 minutes. Pollinators that were easily identifiable (e.g. honeybees and Bombus funebris, medium and big hoverflies) were recorded without capturing when possible."

# Sampling info
insect_sampling$total_sampled_area <- 150*2*3
insect_sampling$total_sampled_time <- 15*2*3
insect_sampling$total_sampled_flowers <- NA

# Notes
insect_sampling$notes <- paste0("Round number: ",insect_sampling$Ronda,
                                ". Height above sea level:",insect_sampling$ALTITUD," m.")

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
  description = insect_sampling$Description,
  notes = insect_sampling$notes

)

# Save new insect sampling templates

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_file, paste0(dir, "insect_sampling_Carlos_Zaragoza_Trello_Vicia_faba_Ecuador_2017.csv"))
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

abundance_aux <- abundance_aux %>% mutate(humbleflies=0,beetles=0,lepidoptera=0,
                                          non_bee_hymenoptera=0,other=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))


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
percentage_species_morphos <- 0

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",
         richness_restriction="bees+flies"
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
  variety = NA,
  management = NA,
  country = data.site$country,
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = NA,
  yield = NA,
  yield_units = NA,
  yield2 = NA,
  yield2_units = NA,
  yield_treatments_no_pollinators = NA,
  yield_treatments_pollen_supplement = NA,
  yield_treatments_no_pollinators2 = NA,
  yield_treatments_pollen_supplement2 = NA,
  fruits_per_plant = NA,
  fruit_weight = NA,
  plant_density = NA,
  seeds_per_fruit = NA,
  seeds_per_plant = NA,
  seed_weight = NA,
  sampling_richness ="transects",
  observed_pollinator_richness = NA,
  other_pollinator_richness = NA,
  other_richness_estimator_method = NA,
  richness_restriction = "bees+flies",
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
  total_sampled_area = 150*2*3,
  total_sampled_time = 15*2*3,
  sampling_visitation = NA,
  visitation_rate_units = NA,
  visitation_rate = NA,
  visit_honeybee = NA,
  visit_bombus = NA,
  visit_wildbees = NA,
  visit_syrphids = NA,
  visit_humbleflies = NA,
  visit_other_flies = NA,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = NA,
  Credit = "Carlos Zaragoza-Trello, Ignasi Bartomeus",
  Email_contact = "carlos.zaragoza@ebd.csic.es"
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, paste0(dir,"field_level_data_Carlos_Zaragoza_Trello_Vicia_faba_Ecuador_2017.csv"))
#setwd(dir_ini)
