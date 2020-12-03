
# This script process the templates in the folder "Your_new_study":
#
# 1) Run test. Only those files without error
# will be used in the next steps.
# If any test fails, a report will be created: Test_Report.pdf
#
# 2) If needed, richness and abundance are estimated
#
# 3) Move the verified template to:
# Processing_files/Datasets_Processing/Templates processed automatically/
#
# 4) Save the processed csv files:
# Processing_files/Datasets_storage/
#
# 5) Update CropPol
#
# 6) Update version history


#########################
# require/load libraries
#########################

source("Processing_files/Datasets_Processing/R-Scripts/import_libraries.R")

#########################
# Process the template
#########################
#
# NOTE 1: If any test fails, a report will be created: Test_Report.pdf
# If not, your file will be moved to the folder
# Processing_files/Datasets_Processing/Templates processed automatically/
# and the resulting csv files will be storaged at
# Processing_files/Datasets_storage/
#
# NOTE 2: Re-compiling CropPol takes around 5 min, due to the cleaning of
# non-ASCII characters in insect_sampling files.

source("Processing_files/Datasets_Processing/R-Scripts/processing_file.R")
