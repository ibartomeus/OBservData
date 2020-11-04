
library(tidyverse)
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Lindstrom_2011
##########################

data_raw <- read.delim("Individual CSV/field_level_data_Sandra_Lindstrom_Brassica_napus_Sweden_2011.csv",
                       header = T,sep = ";")

data_raw$yield_units <- "tonnes/ha"


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data_raw, "field_level_data_Sandra_Lindstrom_Brassica_napus_Sweden_2011.csv")
setwd(dir_ini)

