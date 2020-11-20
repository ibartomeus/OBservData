
library(tidyverse)

dir_ini <- getwd()


field_level_data <- read_csv("DATASETS/field_level_data_Heather_Lee_Grab_Fragaria_ananassa_USA_2012_ed.csv")

# Fix variety

field_level_data$variety %>% unique()

field_level_data$variety[field_level_data$variety=="\"Jewel\""] <- "Jewel"
field_level_data$variety[field_level_data$variety=="\"Unknown -Brreeding Plot\""] <- "Unknown -Brreeding Plot"
field_level_data$variety[field_level_data$variety=="\"Sparkle\""] <- "Sparkle"
field_level_data$variety %>% unique()



# Fix sampling months

field_level_data$sampling_start_month %>% unique()
field_level_data$sampling_end_month %>% unique()

field_level_data$sampling_start_month <- 5
field_level_data$sampling_end_month  <- 6


# Richness with beebowls

field_level_data$observed_pollinator_richness <- NA
field_level_data$other_pollinator_richness <- NA
field_level_data$other_richness_estimator_method <- NA

field_level_data$richness_restriction <- "only bees. Data was obtained by using bee bowls"

# Add a note in CropPol about this decision

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Heather_Lee_Grab_Fragaria_ananassa_USA_2012.csv")
setwd(dir_ini)

