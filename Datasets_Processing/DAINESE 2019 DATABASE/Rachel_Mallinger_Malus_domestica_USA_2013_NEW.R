
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
# Load feedback from Rachel
##########################

field_level_data <- read_csv("DATASETS/field 2013.csv")
insect_sampling <- read_csv("DATASETS/IS 2013.csv")

# Update credit and contact info
field_level_data$Credit <- "Rachel Mallinger, University of Florida" 
field_level_data$Email_contact <- "rachel.mallinger@ufl.edu"


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Rachel_Mallinger_Malus_domestica_USA_2013.csv")
setwd(dir_ini)
                               

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Rachel_Mallinger_Malus_domestica_USA_2013.csv")
setwd(dir_ini)

