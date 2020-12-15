
# Processing ownership files.
# 1) Specific test for ownership templates. Only those files without error
# will be used in the next steps.
# 2) Extract ownership data without errors


############
# FUNCTIONS

# extract_template_i: function that reads ownership files

extract_template_i <- function(file_name){
  ownership_i <- read_csv(file_name,
                            col_types = cols(

                              study_id = col_character(),
                              name = col_character(),
                              affiliation = col_character(),
                              email = col_character(),
                              role = col_character(),
                              funding = col_character()))
  ownership_i
}

######################################
# 0 List of ownership files
######################################

# List of files (in our data repository folder: Processing_files/Datasets_storage)
folder_base <- "Processing_files/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "field_level_data" and have less than 65 columns

list_files_ownership <- files_base[grepl("ownership", files_base)]

##########################################
# 1 Test data files and save the results
##########################################

# Run test and extract report on files which contain failures
report <- capture_output_lines({
  test_file("testthat/test-format-ownership.R", reporter = "summary")
})

file.failures <- str_match(report, "file_(.*?).csv")
file.failures <- list_files_ownership[
  as.numeric(file.failures[!is.na(file.failures[,1]),2])]
file.failures <- file.failures[!duplicated(file.failures)]


###########################################
# 2 MERGE OWNERSHIP DATA WITHOUT ERRORS
###########################################

# Merge the files without failures

list_files_ownership <- list_files_ownership[!list_files_ownership %in% file.failures]

for (i in seq(length(list_files_ownership))) {

  file_ownership_i <- paste(folder_base, list_files_ownership[i], sep = "/")
  ownership_i <- extract_template_i(file_ownership_i)

  if (i == 1) {
    FINAL_ownership_data <- ownership_i
  }
  else {
    FINAL_ownership_data <- FINAL_ownership_data %>% bind_rows(ownership_i)
  }

}

# Fix study_id Schüepps -> Schüepps

FINAL_ownership_data$study_id[grep("Christof_Sch",FINAL_ownership_data$study_id)] <-
  "Christof_Schuepp_Prunus_avium_Switzerland_2011"
