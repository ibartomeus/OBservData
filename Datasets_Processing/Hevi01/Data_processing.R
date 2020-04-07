
library(tidyverse)
library(openxlsx)
library(sp) #Transforming latitude and longitude

##########################
#Data
##########################

field_level_data <- read.xlsx("Crop_pollination_database_Hevia et al.xlsx",
                          sheet = "field_level_data", startRow = 1)
field_level_data <- as_tibble(field_level_data)
field_level_data_new <- field_level_data

field_level_data_new <- field_level_data %>%
  rename(field_size = field.size,
         fruits_per_plant = mean_fruits_per_plant,
         richness_estimator_method = richness_estimator_.Method)

field_level_data_new$management <- "conventional"
field_level_data_new$crop <- "Helianthus annuus"
field_level_data_new$study_id <- "Hevi01"

# Convert Latitude/Longitude from degrees min sec to decimal

chd = substr(field_level_data$latitude, 3, 3)[1]
chm = substr(field_level_data$latitude, 6, 6)[1]
chs = substr(field_level_data$latitude, 11, 11)[1]

cd = char2dms(field_level_data$latitude,chd=chd,chm=chm,chs=chs)
field_level_data_new$latitude <- as.numeric(cd)

chd = substr(field_level_data$longitude, 2, 2)[1]
chm = substr(field_level_data$longitude, 5, 5)[1]
chs = substr(field_level_data$longitude, 10, 10)[1]

cd = char2dms(field_level_data$longitude,chd = chd,chm = chm,chs = chs)
field_level_data_new$longitude <- as.numeric(cd)

setwd("C:/Users/USUARIO/Desktop/Projects/Observ/Datasets_storage")
write_csv(field_level_data_new, "field_level_data_Hevi01.csv")
