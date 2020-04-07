
library(tidyverse)
library(openxlsx)
#library(sp) #Transforming latitude and longitude

##########################
#Data: SIN PANTRAP!!!!!!!!!!
##########################

field_level_data <- read.xlsx("Crop_pollination_database_JessicaKnapp.xlsx",
                          sheet = "field_level_data", startRow = 1)
field_level_data <- as_tibble(field_level_data)
field_level_data_new <- field_level_data %>%
  rename(field_size = field.size,total_yield = `total_yield(KG/HA)`,
         fruits_per_plant = mean_fruits_per_plant,
         richness_estimator_method = `richness_estimator_.Method`)
field_level_data_new$sampling_year <- 2016
field_level_data_new$richness_estimator_method <- "observed"
field_level_data_new$management <- "conventional"
field_level_data_new$total_sampled_area <- 600
field_level_data_new$ab_wildbees <- NA # Aquí no están los datos de pantrap
field_level_data_new$ab_syrphids <- NA # Aquí no están los datos de pantrap
field_level_data_new$study_id <- "Knap01"
field_level_data_new$Publication <- "10.1016/j.baae.2018.09.003"

setwd("C:/Users/USUARIO/Desktop/Projects/Observ/Datasets_storage")
write_csv(field_level_data_new, "field_level_data_Knap01.csv")
