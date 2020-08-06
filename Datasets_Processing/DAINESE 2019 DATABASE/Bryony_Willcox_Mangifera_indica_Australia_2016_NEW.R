
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Radewill01: will01
##########################


field_level_data <- read_csv("DATASETS/Copy of field_level_data_Bryony_Willcox_Mangifera_indica_Australia_2016_BKW.csv")

field_level_data$Publication <- "10.1038/s41598-019-49535-w"

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Bryony_Willcox_Mangifera_indica_Australia_2016.csv")
setwd(dir_ini)
