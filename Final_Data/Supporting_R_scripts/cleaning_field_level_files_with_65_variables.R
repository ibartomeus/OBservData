
# Processing field_level_data files with 65 variables:
# 1) Specific test for field level files with 65 variables. Only those files without error
# will be used in the next steps.
# 2) Extract field level data (with 65 variables) without errors
# 3) Merge such data with field_level_data files processed above
# (i.e., files that had initially 61 columns)


############
# FUNCTIONS

# extract_template_i_65: function that reads field_level_data files with 65 variables

extract_template_i_65 <- function(file_name){
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
                              sampling_richness = col_character(),
                              observed_pollinator_richness = col_double(),
                              other_pollinator_richness = col_double(),
                              other_richness_estimator_method = col_character(),
                              richness_restriction = col_character(),
                              sampling_abundance = col_character(),
                              abundance = col_double(),ab_honeybee = col_double(),
                              ab_bombus = col_double(),ab_wildbees = col_double(),
                              ab_syrphids = col_double(),ab_humbleflies = col_double(),
                              ab_other_flies = col_double(),ab_beetles = col_double(),
                              ab_lepidoptera = col_double(),ab_nonbee_hymenoptera = col_double(),
                              ab_others = col_double(),
                              total_sampled_area = col_double(),
                              total_sampled_time = col_double(),
                              sampling_visitation = col_character(),
                              visitation_rate_units = col_character(),
                              visitation_rate = col_double(),visit_honeybee = col_double(),
                              visit_bombus = col_double(),visit_wildbees = col_double(),
                              visit_syrphids = col_double(),visit_humbleflies = col_double(),
                              visit_other_flies = col_double(),visit_beetles = col_double(),
                              visit_lepidoptera = col_double(),visit_nonbee_hymenoptera = col_double(),
                              visit_others = col_double(),
                              Publication = col_character(),
                              Credit = col_character(),Email_contact = col_character(),
                              notes = col_character()))
  field_level_i
}

##########################################
# 1 Test data files and save the results
##########################################

# Run test and extract report on files which contain failures
report <- capture_output_lines({
  test_file("testthat/test-format-field_level-65.R", reporter = "summary")
})

file.failures <- str_match(report, "file_(.*?).csv")
file.failures <- list_all_files_field_level[
  as.numeric(file.failures[!is.na(file.failures[,1]),2])]
file.failures <- file.failures[!duplicated(file.failures)]

###########################################
# 2 MERGE FIELD_LEVEL DATA WITHOUT ERRORS
###########################################

# Select those files without failures

list_files_field_level_65 <-
  list_files_field_level_65[!list_files_field_level_65 %in% file.failures]

# Merge the files without failures

if(length(list_files_field_level_65)>0){

  for (i in seq(length(list_files_field_level_65))) {

    file_field_level_i <- paste(folder_base, list_files_field_level_65[i], sep = "/")
    field_level_i <- extract_template_i_65(file_field_level_i)

    FINAL_field_level_data_filt <- FINAL_field_level_data_filt %>%
      bind_rows(field_level_i)
  }

}
