
library(tidyverse)

field_level_data <- read_csv("Processing_files/Datasets_processing/REILLY 2020 DATABASE/datasets/CropPol_field_level_data addition Jamie_Ellis_Citrullus_lanatus_USA_2014.csv")

write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Jamie_Ellis_Citrullus_lanatus_USA_2014.csv")
