library(tidyverse)

dir_ini <- getwd()

##########################
#Data: DAINESE, Garr01: garr03
##########################
insect_sampling <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage/insect_sampling_Michael_Garratt_Malus_domestica_UK_2011.csv")

# NEW INFO
# In all surveys 6 transects were carried out on each survey. 
# For beans, oilseed and strawberries 3 survey rounds were carried
# out and for apples 2 survey rounds were done. Each 50m transect was walk over 10 mins.

insect_sampling$total_sampled_area[insect_sampling$sampling_method=="transect"] <- NA
insect_sampling$total_sampled_time[insect_sampling$sampling_method=="transect"] <- 6*2*10
insect_sampling$Description <- "Pollinators were sampled in 6 pan trap/transects in 2 sampling rounds. In all surveys transects were 50m-long and each transect was walk over 10 mins."


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Michael_Garratt_Malus_domestica_UK_2011.csv")
setwd(dir_ini)


field_level_data <- read_csv("DATASETS/field_level_data_Michael_Garratt_Malus_domestica_UK_2011_MG.csv")

field_level_data$total_sampled_area <- NA
field_level_data$total_sampled_time <- 6*2*10
field_level_data$Credit <- "Michael Garratt and Simon Potts(University of Reading)"

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Michael_Garratt_Malus_domestica_UK_2011.csv")
setwd(dir_ini)

