
library(tidyverse)

dir_ini <- getwd()

##########################
#Data: DAINESE, ekro01
##########################

field_level_data <- read.delim("DATASETS/field_level_data_Johan-Ekroos_Vicia_faba_Sweden_2016_JE.csv",sep=";")

field_level_data$sampling_start_month <- 6
field_level_data$sampling_end_month <- 7


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Johan_Ekroos_Vicia_faba_Sweden_2016.csv")
setwd(dir_ini)
