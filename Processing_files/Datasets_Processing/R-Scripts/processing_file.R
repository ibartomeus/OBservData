
# This script processes the templates in the folder "Your_new_study" and
# updates CropPol:
# 1) Run test. Only those files without error
# will be used in the next steps.
# 2) If needed, richness and abundance are estimated
# (records where guild is equal to NA will not be considered)
# 3) Move the verified template to:
# Processing_files/Datasets_Processing/Templates processed automatically/
# 4) Save the processed csv files:
# Processing_files/Datasets_storage/
# 5) Update CropPol
# 6) Update version history


##########################################
# LOAD DATA
##########################################

folder_new_study <- "Your_new_study"

# Load data

files <- list.files(folder_new_study)
excel_file <- files[grep(".xlsx",files)]
ods_file <- files[grep(".ods",files)]

if(length(excel_file)>0){

  data.site <- read_excel(paste0("Your_new_study/",excel_file),
                          sheet = "field_level_data")
  data.insect <- read_excel(paste0("Your_new_study/",excel_file),
                            sheet = "insect_sampling")
  data.sampling <- read_excel(paste0("Your_new_study/",excel_file),
                              sheet = "sampling_method")
  data.ownership <- read_excel(paste0("Your_new_study/",excel_file),
                               sheet = "ownership")


}else{

  data.site <- read_ods(paste0("Your_new_study/",ods_file),
                        sheet = "field_level_data") %>% as_tibble()
  data.insect <- read_ods(paste0("Your_new_study/",ods_file),
                          sheet = "insect_sampling") %>% as_tibble()
  data.sampling <- read_ods(paste0("Your_new_study/",ods_file),
                            sheet = "sampling_method") %>% as_tibble()
  data.ownership <- read_ods(paste0("Your_new_study/",ods_file),
                            sheet = "ownership") %>% as_tibble()
}


##########################################
# 1 Test data files
##########################################

# If any test fails, a report will be created: Test_Report.pdf

source("Processing_files/Datasets_Processing/R-Scripts/run_tests.R")
{stopifnot(TRUE, template_OK)
#######################################################################
# 2.1 If insect sampling data is provided, abundance is estimated
# by using all the sampling methods that are compatible with such task.
#######################################################################

methods_abundance <- data.sampling %>% filter(sampling_abundance==1) %>%
  select(sampling_method) %>% unique() %>% pull()

methods_abundance_type <- data.sampling %>% filter(sampling_abundance==1) %>%
    select(type) %>% unique() %>% pull()

if(length(methods_abundance)>0){
  source("Processing_files/Datasets_Processing/R-Scripts/estimate_abundace.R")
}

#######################################################################
# 2.2 If insect sampling data is provided, richness is estimated
# by using all the sampling methods that are compatible with such task.
#######################################################################

methods_richness <- data.sampling %>% filter(sampling_richness==1) %>%
  select(sampling_method) %>% unique() %>% pull()

methods_richness_type <- data.sampling %>% filter(sampling_richness==1) %>%
  select(type) %>% unique() %>% pull()

if(length(methods_richness)>0){
  source("Processing_files/Datasets_Processing/R-Scripts/estimate_richness.R")
}

##########################
# 3 Save verified template
##########################

if(length(excel_file)>0){

  file_OK <- excel_file
  file_path <- paste0("Your_new_study/",excel_file)
  new_file_path <- paste0(
    "Processing_files/Datasets_Processing/Templates processed automatically/",excel_file)

}else{
  file_OK <- ods_file
  file_path <- paste0("Your_new_study/",ods_file)
  new_file_path <- paste0(
    "Processing_files/Datasets_Processing/Templates processed automatically/",ods_file)

}


file.copy(from=file_path, to = new_file_path,
          overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)

# Remove original file
file.remove(file_path)

# If a prior test report exits, remove it
if (file.exists("Your_new_study/Test_Report.pdf")) {
  #Delete report if it exists
  file.remove("Your_new_study/Test_Report.pdf")
}

###############################
# 4 Save csv files
###############################

file_name_OK <- (strsplit(file_OK, ".", fixed = TRUE))[[1]][1]

field_path <- paste0("Processing_files/Datasets_storage/field_level_data_",file_name_OK,".csv")
insect_path <- paste0("Processing_files/Datasets_storage/insect_sampling_",file_name_OK,".csv")
ownership_path <- paste0("Processing_files/Datasets_storage/data_ownership_",file_name_OK,".csv")

write_csv(data.site, field_path)
write_csv(data.insect, insect_path)
write_csv(data.ownership, ownership_path)

################################
# 5 Update CropPol
################################

source("Final_Data/Compiling_CropPol_verified_data.R")

################################
# 6 Update New
################################

source("Processing_files/Datasets_Processing/R-Scripts/update_new.R")

}
