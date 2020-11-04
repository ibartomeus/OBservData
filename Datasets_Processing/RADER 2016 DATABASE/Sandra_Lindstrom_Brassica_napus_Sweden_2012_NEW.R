
library(tidyverse)


dir_ini <- getwd()

##########################
#Data: Lindstrom_2012
##########################

data_raw <- read.delim("Individual CSV/field_level_data_Sandra_Lindstrom_Brassica_napus_Sweden_2012.csv",
                       header = T,sep = ";")

data_raw <- data_raw[1:22,]

data_raw$yield_units <- "tonnes/ha"


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data_raw, "field_level_data_Sandra_Lindstrom_Brassica_napus_Sweden_2012.csv")
setwd(dir_ini)

