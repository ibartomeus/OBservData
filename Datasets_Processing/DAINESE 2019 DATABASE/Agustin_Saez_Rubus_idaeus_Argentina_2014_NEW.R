
library(tidyverse)

dir_ini <- getwd()

field_level_data <- read_csv("DATASETS/field_level_data_Agustin_Saez_Rubus_idaeus_Argentina_2014.csv")
field_level_data$fruit_weight <- field_level_data$yield2

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Agustin_Saez_Rubus_idaeus_Argentina_2014.csv")
setwd(dir_ini)

