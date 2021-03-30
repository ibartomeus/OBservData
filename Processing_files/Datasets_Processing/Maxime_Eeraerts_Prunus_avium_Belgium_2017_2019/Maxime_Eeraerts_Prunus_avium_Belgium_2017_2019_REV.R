
# load libraries
library(tidyverse)

dir_ini <- getwd()

# Load data
dir <- "Processing_files/Datasets_Processing/Maxime_Eeraerts_Prunus_avium_Belgium_2017_2019/"
file <- paste0(dir,"field_level_data_Maxime_Eeraerts_Prunus_avium_Belgium_2017_2019_REV.csv")

data.site <- read_csv(file)

data.site$study_id[data.site$sampling_year==2015] <- "Eeraerts_etal_sweetcherry_Belgium_2015"
data.site$study_id[data.site$sampling_year==2016] <- "Eeraerts_etal_sweetcherry_Belgium_2016"
data.site$study_id[data.site$sampling_year==2017] <- "Eeraerts_etal_sweetcherry_Belgium_2017"

write_csv(data.site,paste0(dir,
                           "field_level_data_Maxime_Eeraerts_Prunus_avium_Belgium_2017_2019_REV2.csv"))

file <- paste0(dir,"insect_sampling_Maxime_Eeraerts_Prunus_avium_Belgium_2017_2019_REV.csv")
insect_sampling <- read_csv(file)
sites_2015 <- data.site$site_id[data.site$sampling_year==2015]
sites_2016 <- data.site$site_id[data.site$sampling_year==2016]
sites_2017 <- data.site$site_id[data.site$sampling_year==2017]
insect_sampling$study_id[insect_sampling$site_id %in% sites_2015] <- "Eeraerts_etal_sweetcherry_Belgium_2015"
insect_sampling$study_id[insect_sampling$site_id %in% sites_2016] <- "Eeraerts_etal_sweetcherry_Belgium_2016"
insect_sampling$study_id[insect_sampling$site_id %in% sites_2017] <- "Eeraerts_etal_sweetcherry_Belgium_2017"

write_csv(insect_sampling,paste0(dir,
                           "insect_sampling_Maxime_Eeraerts_Prunus_avium_Belgium_2017_2019_REV2.csv"))
