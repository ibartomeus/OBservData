
library(tidyverse)


dir_ini <- getwd()

##########################
#Data: Bryony Willcox
##########################

# Now it contains avo_5 data for 2016 and 2017, with their respective values of
# richness, abundance, yield, yield_treatments_pollen_supplement, 
# and fruit_per_plant information.

field_level_data <- read_csv("Copy of field_level_data_Bryony_Willcox_several_crops_Australia_several_years_BW.csv")

field_level_data$study_id[field_level_data$study_id=="Bryony_Willcox_Mangifera_indica_Australia_2016"] <- "Bryony_Willcox_Mangifera_indica_Australia_2016_2"

insect_sampling <-
  read_csv("NEW_insect_sampling_Bryony_Willcox_several_crops_Australia_several_years.csv")

insect_sampling$study_id[insect_sampling$study_id=="Bryony_Willcox_Mangifera_indica_Australia_2016"] <- "Bryony_Willcox_Mangifera_indica_Australia_2016_2"

#######################
# Save
#######################


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data,"field_level_data_Bryony_Willcox_several_crops_Australia_several_years.csv")
write_csv(insect_sampling,"insect_sampling_Bryony_Willcox_several_crops_Australia_several_years.csv")
setwd(dir_ini)
