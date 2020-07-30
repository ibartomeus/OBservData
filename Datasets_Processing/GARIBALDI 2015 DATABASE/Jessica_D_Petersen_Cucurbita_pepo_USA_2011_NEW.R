# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


field_level_data <- read_csv("Nacho-Lucas/datos/field_level_data_Jessica_D_Petersen_Cucurbita_pepo_USA_2011.csv")
field_level_data <- as_tibble(field_level_data)

# Fix management

field_level_data$management <- "conventional"

field_level_data <- mutate(field_level_data, abundance = ab_honeybee + ab_bombus + ab_wildbees)


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Jessica_D_Petersen_Cucurbita_pepo_USA_2011.csv")
setwd(dir_ini)

# 