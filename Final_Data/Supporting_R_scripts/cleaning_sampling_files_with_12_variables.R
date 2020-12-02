
# Processing insect_sampling files with 65 variables:
# 1) Specific test for sampling files with 65 variables. Only those files without
# errors will be used in the next steps.
# 2) Extract insect_sampling data (with 12 variables) without errors
# 3) Merge such data with the insect_sampling files processed above
# (i.e., the files that had initially 10 columns)

############
# FUNCTIONS

# extract_sampling_i_12: function that reads insect_sampling files with 12 variables


extract_sampling_i_12 <- function(file_name){
  sampling_i <- read_csv(file_name,
                         col_types = cols(
                           study_id = col_character(),
                           site_id = col_character(),
                           sampling_method = col_character(),
                           pollinator = col_character(),
                           identified_to = col_character(),
                           guild = col_character(),
                           abundance = col_double(),
                           total_sampled_area = col_double(),
                           total_sampled_time = col_double(),
                           total_sampled_flowers = col_double(),
                           description = col_character(),
                           notes = col_character()
                         ))
  sampling_i
}



############################################
# 1 Test for sampling files with 10 variables
############################################

report <- capture_output_lines({
  test_file("testthat/test-format-insect_sampling-12.R", reporter = "summary")
})

# Extract the names of the files which contain failures

file.failures <- str_match(report, "file_(.*?).csv")
file.failures <- list_all_files_insect_sampling[
  as.numeric(file.failures[!is.na(file.failures[,1]),2])]
file.failures <- file.failures[!duplicated(file.failures)]


##############################################
# 2-3 SELECT AND MERGE INSECT_SAMPLING DATA WITHOUT ERRORS
##############################################

# Select those files without failures

list_files_insect_sampling_12 <- list_files_insect_sampling_12[!list_files_insect_sampling_12 %in% file.failures]

# Merge the files without failures

if(length(list_files_insect_sampling_12) > 0){

  for (i in seq(length(list_files_insect_sampling_12))) {

    file_sampling_i <- paste(folder_base, list_files_insect_sampling_12[i], sep = "/")
    sampling_i <- extract_sampling_i_12(file_sampling_i)

    insect_sampling_taxa <- insect_sampling_taxa %>%
      bind_rows(sampling_i)
  }

}
