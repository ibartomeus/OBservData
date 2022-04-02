library(tidyverse)
list_studies <- read_csv("Processing_files/Datasets_Processing/REILLY 2020 DATABASE/datasets/ICP_sites_with_latlong_for_observ.csv")

state_crop <- list_studies$crop_id %>% unique()

ICP_new_IDS <- tibble(crop_state_combination=state_crop,
                            corresponding_author=NA)

write_csv(ICP_new_IDS,"Processing_files/Datasets_Processing/REILLY 2020 DATABASE/datasets/ICP_new_IDS.csv")

final_field <- read_csv("Final_Data/Datasets_Processing/REILLY 2020 DATABASE/datasets/ICP_sites_with_latlong_for_observ.csv")
