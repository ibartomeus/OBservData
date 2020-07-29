
library(tidyverse)

dir_ini <- getwd()

##########################
#Data: DAINESE, Garr01: garr03
##########################
insect_sampling <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage/insect_sampling_Michael_Garratt_Brassica_napus_UK_2012.csv")

# NEW INFO
# In all surveys 6 transects were carried out on each survey. 
# For beans, oilseed and strawberries 3 survey rounds were carried
# out and for apples 2 survey rounds were done. Each 50m transect was walk over 10 mins.

insect_sampling$total_sampled_area[insect_sampling$sampling_method=="transect"] <- 6*3*50*2
insect_sampling$total_sampled_time[insect_sampling$sampling_method=="transect"] <- 6*3*10
insect_sampling$Description <- "Measures per site: fruit set & size, seed set & size measured once on 10 plants in exclosures (1 open 1 closed, 10 treatment reps per site); pollinators sampled in 6 pan trap/transects in 3 sampling rounds; visitation rates in 6 2m² quadrats in 3 sampling rounds. In all surveys transects were 50m-l x 2m-w (100 m2) and each transect was walk over 10 mins."


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Michael_Garratt_Brassica_napus_UK_2012.csv")
setwd(dir_ini)


field_level_data <- read_csv("DATASETS/field_level_data_Michael_Garratt_Brassica_napus_UK_2012_MG.csv")

field_level_data$total_sampled_area <- 6*3*50*2
field_level_data$total_sampled_time <- 6*3*10
field_level_data$Credit <- "Michael Garratt and Simon Potts(University of Reading)"

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Michael_Garratt_Brassica_napus_UK_2012.csv")
setwd(dir_ini)

