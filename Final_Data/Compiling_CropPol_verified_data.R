
# Merge individual "field_level_data" and "insect_sampling_data"  files
# that are in Processing_files/Data_sets_storage, respectively, if and only
# if the corresponding files tests are OK and if the files have been checked by
# their corresponding authors.

library(tidyverse)
library(testthat) # for data tests
library(readxl)
library(tools) # for removing non-ASCII characters


# Update the field_level_data csv files without richness_restriction variable
# (Files extracted from Dainese and Rader meta-analyses, and other data sources)
source("Final_Data/Supporting_R_scripts/add_taxon_constraint_column_DAINESE_RADER_OTHER.R",
       encoding="utf-8")

# Processing field_level_data files with only 61 variables
source("Final_Data/Supporting_R_scripts/cleaning_field_level_files_with_61_variables.R",
       encoding="utf-8")

# Processing field_level_data files with 65 variables
source("Final_Data/Supporting_R_scripts/cleaning_field_level_files_with_65_variables.R",
       encoding="utf-8")

# Add the dynamic paper variables to field_level_data files with 65 variables

# Processing field_level_data files with 65 variables
source("Final_Data/Supporting_R_scripts/cleaning_field_level_files_with_68_variables.R",
       encoding="utf-8")

# Save "total_field_level_data" file
write.csv(FINAL_field_level_data_for_dynamic_web_reordered, "Final_Data/CropPol_field_level_data.csv",
          row.names = F)

# Processing insect_sampling files with only 10 variables
# NOTE: Cleaning diacritic (non-ASCII) characters takes a while
source("Final_Data/Supporting_R_scripts/cleaning_sampling_files_with_10_variables.R",
       encoding="utf-8")

# Processing insect_sampling files with 12 variables
source("Final_Data/Supporting_R_scripts/cleaning_sampling_files_with_12_variables.R",
       encoding="utf-8")

# Save "FINAL_sampling_data" file
write.csv(insect_sampling_taxa, "Final_Data/CropPol_sampling_data.csv",
          row.names = F)

# Processing insect_sampling files with 12 variables
source("Final_Data/Supporting_R_scripts/cleaning_data_ownership.R",
       encoding="utf-8")

# Save " FINAL_ownership_data" file
write.csv(FINAL_ownership_data, "Final_Data/CropPol_data_ownership.csv",
          row.names = F)
