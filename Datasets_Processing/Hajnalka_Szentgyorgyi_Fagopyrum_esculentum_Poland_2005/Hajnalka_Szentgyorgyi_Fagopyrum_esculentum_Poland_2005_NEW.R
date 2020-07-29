# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

# list of files
list.files()


#################
# INSECT SAMPLING
#################
insect_sampling <- openxlsx::read.xlsx("SzH_insect_sampling_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.xlsx")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.csv")
setwd(dir_ini)




###############################
# FIELD LEVEL DATA
###############################

# Field level abundance and richness is updated


field_level_data <- openxlsx::read.xlsx("SzH_field_level_data_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.xlsx")

# Fix latitude and longitude

field_level_data$latitude <- parse_lat(field_level_data$latitude)
field_level_data$longitude <- parse_lon(field_level_data$longitude)

# Fix richness abundances




setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.csv")
setwd(dir_ini)

