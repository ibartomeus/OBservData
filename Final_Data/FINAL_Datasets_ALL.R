
# Merge individual "field_level_data" and "insect_sampling_data"  files, respectively,
# if the corresponding files tests are OK.

library(tidyverse)
library(testthat)


folder_base <- "../Datasets_storage"

files_base <- list.files(folder_base)


#Date: 11/05/2020 -> Version: 0.1
#Date: 27/10/2020 -> Version: 0.2

##############################
# MERGE FIELD_LEVEL DATA
##############################


# List of files (in our data repository folder) whose name begins with
# "field_level_data"

list_files_field_level <- files_base[grepl("field_level_data", files_base)]

# extract_template_i: function that merges the dataset files
# fiel_level_ data: file that contains the whole dataset

extract_template_i <- function(file_name){
  field_level_i <- read_csv(file_name,
                            col_types = cols(
                              study_id = col_character(),
                              site_id = col_character(),
                              crop = col_character(),
                              variety = col_character(),management = col_character(),
                              country = col_character(),latitude = col_double(),
                              longitude = col_double(),X_UTM = col_double(),
                              Y_UTM = col_double(),zone_UTM = col_character(),
                              sampling_start_month = col_double(),
                              sampling_end_month = col_double(),
                              field_size = col_double(),sampling_year = col_character(),
                              yield = col_double(),
                              yield_units = col_character(),
                              yield2 = col_double(),yield2_units = col_character(),
                              yield_treatments_no_pollinators = col_double(),
                              yield_treatments_pollen_supplement = col_double(),
                              yield_treatments_no_pollinators2 = col_double(),
                              yield_treatments_pollen_supplement2 = col_double(),
                              fruits_per_plant = col_double(),fruit_weight = col_double(),
                              plant_density = col_double(),seeds_per_fruit = col_double(),
                              seeds_per_plant = col_double(),seed_weight = col_double(),
                              observed_pollinator_richness = col_double(),
                              other_pollinator_richness = col_double(),
                              other_richness_estimator_method = col_character(),
                              richness_restriction = col_character(),
                              abundance = col_double(),ab_honeybee = col_double(),
                              ab_bombus = col_double(),ab_wildbees = col_double(),
                              ab_syrphids = col_double(),ab_humbleflies = col_double(),
                              ab_other_flies = col_double(),ab_beetles = col_double(),
                              ab_lepidoptera = col_double(),ab_nonbee_hymenoptera = col_double(),
                              ab_others = col_double(),
                              total_sampled_area = col_character(),
                              total_sampled_time = col_double(),
                              visitation_rate_units = col_character(),
                              visitation_rate = col_double(),visit_honeybee = col_double(),
                              visit_bombus = col_double(),visit_wildbees = col_double(),
                              visit_syrphids = col_double(),visit_humbleflies = col_double(),
                              visit_other_flies = col_double(),visit_beetles = col_double(),
                              visit_lepidoptera = col_double(),visit_nonbee_hymenoptera = col_double(),
                              visit_others = col_double(),
                              Publication = col_character(),
                              Credit = col_character(),Email_contact = col_character()))
  field_level_i
}

#Test data files and save the results

test_file("../testthat/test-format-field_level.R", reporter = "summary") #Visualize testing
options(testthat.output_file = "../testthat/test_out_Create_Dataset.txt")
# test_file("../testthat/test-format.R", reporter = "summary")

# Extract the names of the files which contain failures

report <- readLines("../testthat/test_out_Create_Dataset.txt")

file.failures <- str_match(report, "field_level_data_(.*?)csv")
file.failures <- file.failures[!is.na(file.failures[,1]),1]
file.failures <- file.failures[!duplicated(file.failures)]

# Merge the files without failures

list_files_field_level <- list_files_field_level[!list_files_field_level %in% file.failures]

for (i in seq(length(list_files_field_level))) {
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  field_level_i <- extract_template_i(file_field_level_i)
  
  if (i == 1) {
    FINAL_field_level_data <- field_level_i
    }
  else {
    FINAL_field_level_data <- FINAL_field_level_data %>% bind_rows(field_level_i)
    }
  
}

# Fix crop names:


FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Malus Domestica"] <- "Malus domestica"
FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Fragaria × ananassa"] <- "Fragaria x ananassa"

# Fix country:
FINAL_field_level_data$country[FINAL_field_level_data$country=="United States"] <- "USA"
FINAL_field_level_data$country %>% unique() %>% sort()

# Fix variety:
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Koipesol Oleko"] <- "Koipesol OLEKO"
FINAL_field_level_data$variety %>% unique() %>% sort()

# Fix pollinator restriction

FINAL_field_level_data$richness_restriction[is.na(FINAL_field_level_data$richness_restriction)] <- "none"
FINAL_field_level_data$richness_restriction[is.na(FINAL_field_level_data$observed_pollinator_richness)&
                                              is.na(FINAL_field_level_data$other_pollinator_richness)&
                                              is.na(FINAL_field_level_data$other_richness_estimator_method)] <- NA


#Fix other_richness_estimator_method

FINAL_field_level_data$other_richness_estimator_method[FINAL_field_level_data$other_richness_estimator_method=="Chao1"] <- "Chao 1"


# Save "total_field_level_data" file

write_csv(FINAL_field_level_data, "../Final_Data/FINAL_field_level_data_V0p2.csv")


##############################
# MERGE INSECT_SAMPLING DATA
##############################

# List of files (in our data repository folder) whose name begins with
# "insect_sampling"

list_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]


extract_sampling_i <- function(file_name){
  sampling_i <- read_csv(file_name,
                            col_types = cols(
                              study_id = col_character(),
                              site_id = col_character(),
                              pollinator = col_character(),
                              guild = col_character(),
                              sampling_method = col_character(),
                              abundance = col_double(),
                              total_sampled_area = col_double(),
                              total_sampled_time = col_double(),
                              total_sampled_flowers = col_double(),
                              Description = col_character()
                              ))
  sampling_i
}

#Test data files and save the results

test_file("../testthat/test-format-insect_sampling.R", reporter = "summary") #Visualize testing
options(testthat.output_file = "../testthat/test_out_Create_Dataset_I.txt")
# test_file("../testthat/test-format.R", reporter = "summary")

# Extract the names of the files which contain failures

report <- readLines("../testthat/test_out_Create_Dataset_I.txt")

file.failures <- str_match(report, "insect_sampling_(.*?)csv")
file.failures <- file.failures[!is.na(file.failures[,1]),1]
file.failures <- file.failures[!duplicated(file.failures)]

# Merge the files without failures

list_files_insect_sampling <- list_files_insect_sampling[!list_files_insect_sampling %in% file.failures]


for (i in seq(length(list_files_insect_sampling))) {
  
  sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  sampling_i <- extract_sampling_i(sampling_i)
  
  if (i == 1) {
    FINAL_sampling_data <- sampling_i
  }
  else {
    FINAL_sampling_data <- FINAL_sampling_data %>% bind_rows(sampling_i)
  }
  
}

FINAL_sampling_data <- FINAL_sampling_data %>% filter(abundance > 0)

FINAL_sampling_data %>% group_by(guild) %>% count()

# fix guilds 

FINAL_sampling_data$guild[FINAL_sampling_data$guild=="bombyliidae"] <- "humbleflies"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="other wild bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="wild_bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="others"] <- "other"

FINAL_sampling_data %>% group_by(guild) %>% count()

# Save "FINAL_sampling_data" file

write_csv(FINAL_sampling_data, "../Final_Data/FINAL_sampling_data_V0p2.csv")

