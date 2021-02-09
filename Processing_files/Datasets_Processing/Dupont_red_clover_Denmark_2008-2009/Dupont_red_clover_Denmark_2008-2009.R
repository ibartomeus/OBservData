
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
dir <- "Processing_files/Datasets_Processing/Dupont_red_clover_Denmark_2008-2009/"
file <- paste0(dir,"Dupont_red_clover_Denmark_2008-2009.ods")

data.site <- read_ods(file, sheet = "field_level_data") %>% as_tibble()
insect_sampling <- read_ods(file, sheet = "insect_sampling") %>% as_tibble()
data.ownership <- read_ods(file, sheet = "ownership") %>% as_tibble()


# Addapt study_id in data.site to OBServ format

data.site$study_id <- paste0("Dupont_redclover_Denmark_",data.site$sampling_year)


# Remove NON-ASCII characters from the field_level template

df <- data.site %>% select(study_id,site_id,Credit)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×øØ", "aeiounuAoacaAENOaxoO",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×øØ", "aeiounuAoacaAENOaxoO",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×øØ", "aeiounuAoacaAENOaxoO",df[r,3])
}

# Check the information
tools::showNonASCII(df$study_id) %>% unique()
tools::showNonASCII(df$site_id) %>% unique()
df$site_id[df$site_id == "Brædstrup_2008"] <- "Braedstrup_2008"
df$site_id[df$site_id == "Brædstrup_2009"] <- "Braedstrup_2009"
tools::showNonASCII(df$site_id) %>% unique()
tools::showNonASCII(df$Credit) %>% unique()

# Add the corrected information to the field_level dataframe
data.site$study_id <- df$study_id
data.site$site_id <- df$site_id
data.site$Credit <- df$Credit

# Remove NON-ASCII characters from the insect_sampling template

df <- insect_sampling %>% select(study_id,site_id)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×øØ", "aeiounuAoacaAENOaxoO",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×øØ", "aeiounuAoacaAENOaxoO",df[r,2])
}

tools::showNonASCII(df$study_id) %>% unique() #OK!
tools::showNonASCII(df$site_id) %>% unique()
df$site_id[df$site_id == "Brædstrup_2008"] <- "Braedstrup_2008"
df$site_id[df$site_id == "Brædstrup_2009"] <- "Braedstrup_2009"
tools::showNonASCII(df$site_id) %>% unique() #OK!

# # Add the corrected information to the insect_sampling dataframe
insect_sampling$study_id <- df$study_id
insect_sampling$site_id <- df$site_id

# Check site_ids

insect_sampling$site_id[!insect_sampling$site_id %in% data.site$site_id]
data.site$site_id[!data.site$site_id %in% insect_sampling$site_id]

############################
# Check Country

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
data.site$country == latlong2country(round(geo_data[,1:2],3)) # Our test can not locate site
# 19 in Denmark when we use more than 3 decimal positions.

# Add crop
data.site$crop <- "Trifolium pratense"

# Fix management
data.site$management %>% unique()

data.site$management[data.site$management=="Organic"] <- "organic"

# Addapt yield_units
data.site$yield_units[data.site$yield_units=="kg/ha"] <- "kg per hectare"
data.site$yield2_units[data.site$yield2_units=="kg/ha"] <- "kg per hectare"

# Addapt publication format
data.site$Publication <- "10.1371/journal.pone.0025172"

###########################
# SAMPLING DATA
###########################

# Fix study_id

year <- insect_sampling %>% dplyr::select(site_id) %>%
  separate(site_id,c("aux","year"),"_")

insect_sampling$study_id <- paste0("Dupont_redclover_Denmark_",year$year)


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
  notes = insect_sampling$notes)

# Save new insect sampling templates

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_file, paste0(dir, "insect_sampling_Dupont_red_clover_Denmark_2008-2009.csv"))
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

abundance_aux <- abundance_aux %>% mutate(humbleflies = 0,beetles = 0,
                                          other_wild_bees = 0, syrphids = 0,
                                          other_flies = 0, non_bee_hymenoptera = 0,
                                          lepidoptera = 0, other = 0,
                                          total = 0)
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
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",
         richness_restriction="only bees"
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
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
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
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, paste0(dir,"field_level_data_Dupont_red_clover_Denmark_2008-2009.csv"))
#setwd(dir_ini)
