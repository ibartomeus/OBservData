
library(tidyverse)
library(openxlsx)
library(parzer) #Transforming latitude and longitude
library(stringr)
library(iNEXT)
###################################
#Data holder: Silvia Castro
###################################

dir_ini <- getwd()
options(digits=14)

data.site <- read.xlsx("Crop_pollination_database_SUNFLOWER_2020-07-29_corrected.xlsx",
                          sheet = "field_level_data", startRow = 1)

data.site <- as_tibble(data.site)

# Check site_id
data.site %>% group_by(site_id,sampling_year) %>% count() %>% filter(n>1)

# Change study IDs

data.site$study_id[data.site$sampling_year==2017] <- "Silvia_Castro_Helianthus_annuus_Spain_2017"
data.site$study_id[data.site$sampling_year==2018] <- "Silvia_Castro_Helianthus_annuus_Spain_2018"

# Fix crop

data.site$crop <- "Helianthus annuus"

# Fix management

data.site$management <- "conventional"

# Convert Latitude/Longitude from degrees min sec to decimal

data.site$latitude <- parse_lat(data.site$latitude)

# when parsing longitude, some NAs appears
which(is.na(parse_lon(data.site$longitude)))
data.site$longitude[which(is.na(parse_lon(data.site$longitude)))]

data.site$longitude[2] <- "-3.56233ºW"
data.site$longitude[15] <- "-3.10787ºW"
data.site$longitude[20] <- "-3.61491ºW"
data.site$longitude[21] <- "-3.56197ºW"
data.site$longitude[26] <- "-4.24806ºW"
data.site$longitude[33] <- "-3.11277ºW"
data.site$longitude <- parse_lon(data.site$longitude)

# Fix yield units

data.site$yield_units <- "kg/ha"

###########################
# SAMPLING DATA
###########################

insect_sampling <- read.xlsx("Crop_pollination_database_SUNFLOWER_2020-07-29_corrected.xlsx", sheet = "insect_sampling")

#Sanity check site_id

data.site$site_id[!data.site$site_id %in% insect_sampling$site_id]
insect_sampling$site_id[!insect_sampling$site_id %in% data.site$site_id]

# New study ID

field_ID_2017 <- data.site %>% filter(sampling_year==2017) %>% select(site_id) %>% pull()
field_ID_2018 <- data.site %>% filter(sampling_year==2018) %>% select(site_id) %>% pull()

insect_sampling$study_id[insect_sampling$site_id %in% field_ID_2017] <- "Silvia_Castro_Helianthus_annuus_Spain_2017"
insect_sampling$study_id[insect_sampling$site_id %in% field_ID_2018] <- "Silvia_Castro_Helianthus_annuus_Spain_2018"

# Sanity check
which(insect_sampling$study_id=="POLLOLE project_Burgos")


#Fix guilds

insect_sampling %>% group_by(guild) %>% count()
insect_sampling$guild[insect_sampling$guild=="nonbee_hymenoptera"] <- "non_bee_hymenoptera"
insect_sampling$guild[insect_sampling$guild=="wildbees"] <- "other_wild_bees"
insect_sampling$guild[insect_sampling$guild=="bombus"] <- "bumblebees"
insect_sampling$guild[insect_sampling$guild=="honeybee"] <- "honeybees"
insect_sampling$guild[insect_sampling$guild=="Lepidoptera"] <- "lepidoptera"
insect_sampling$guild[insect_sampling$guild=="others "] <- "others"
insect_sampling %>% group_by(guild) %>% count()

# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description[insect_sampling$sampling_method=="census"] <- insect_sampling$Column1[insect_sampling$sampling_method=="census"]

# Modify pan trap sampling area + total sampling flowers

insect_sampling$total_sampled_area[insect_sampling$sampling_method=="pan-traps"] <- NA
insect_sampling$total_sampled_flowers[insect_sampling$sampling_method=="pan-traps"] <- NA

# Save new insect sampling templates

insect_sampling_2017_aux <- insect_sampling %>% 
  filter(study_id=="Silvia_Castro_Helianthus_annuus_Spain_2017",
         !is.na(abundance), abundance>0) %>% select(-Column1)

# Add a missing raw 4 honeybees at site PRE-NGI2_2017

new_row <- tibble(
  study_id="Silvia_Castro_Helianthus_annuus_Spain_2017",
  site_id="PRE-NGI2_2017",
  pollinator="Apis mellifera",
  guild="honeybees",
  sampling_method="census",
  abundance=4,
  total_sampled_area=711,
  total_sampled_time=316,
  total_sampled_flowers=2526,
  Description="census of 1 min, within the field each in areas of aprox. 1.5x1.5m"
)

# Add the new column to the total insect sampling data and 2017 data
insect_sampling_2017_aux$total_sampled_area <- as.numeric(insect_sampling_2017_aux$total_sampled_area)
insect_sampling_2017_aux$total_sampled_flowers <- as.numeric(insect_sampling_2017_aux$total_sampled_flowers)
insect_sampling_2017 <- bind_rows(insect_sampling_2017_aux,new_row)

insect_sampling$total_sampled_area <- as.numeric(insect_sampling$total_sampled_area)
insect_sampling$total_sampled_flowers <- as.numeric(insect_sampling$total_sampled_flowers)

insect_sampling <- bind_rows(insect_sampling,new_row)

####################

x <- insect_sampling %>% group_by(study_id,site_id,pollinator,guild,sampling_method) %>% count() %>%
  filter(n>1)
