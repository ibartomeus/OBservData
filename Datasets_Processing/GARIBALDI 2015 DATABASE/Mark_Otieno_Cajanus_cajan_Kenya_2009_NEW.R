# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


field_level_data <- read_csv("Nacho-Lucas/datos/field_level_data_Mark_Otieno_Cajanus_cajan_Kenya_2009_19 July 2020.csv")

field_level_data <- field_level_data[1:12,]

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Mark_Otieno_Cajanus_cajan_Kenya_2009.csv")
setwd(dir_ini)


# Abundance data suggest that average values were obtained by dividing over 11.
# According to the paper's methods, the number of transects per field should be a multiple of 10.

