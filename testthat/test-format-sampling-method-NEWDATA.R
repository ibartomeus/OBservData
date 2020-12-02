library(testthat)
library(tidyverse)
library(readr)
library(readxl)
library(readODS)
library(tools)

context("Verification that the values in the sampling_method template meet our requirements")


# Test the features of datasets

# Right labels (dataframe's labels are equal to those of "sampling_method" template)
# Right number of columns (dataframe's number of columns is equal to that of "sampling_method" template)
# All studies are identified (study_ID is not NA)
# sampling_richness format  (0,1)
# sampling_abundance format  (0,1)
# all study IDs are also in the insect_sampling template
# all sampling methods are also in the insect_sampling template
# type categories are in this list: "sweep net","transects",
# "focal observations","pan trap, bee bowl, blue vane trap, pitfall","other"
# Only ASCII characters are allowed.

# all site IDs are also in field_level template

labels_OK <- c("study_id","sampling_method",
               "type",
               "sampling_richness","sampling_abundance","notes")

type_OK <- c("sweep net","transects","focal observations",
             "pan trap, bee bowl, blue vane trap, pitfall","other")

exp_column_number <- length(labels_OK)

folder_base <- "../Your_new_study"

files <- list.files(folder_base)
excel_file <- files[grep(".xlsx",files)]
ods_file <- files[grep(".ods",files)]

if(length(excel_file)>0){
  list_files_sampling_method <- excel_file
}else{
  list_files_sampling_method <- ods_file
  }

for (i in seq(length(list_files_sampling_method))) {

  if(length(excel_file)>0){

    file_sampling_method_i <- paste(folder_base, list_files_sampling_method[i], sep = "/")
    sampling_method_i <- read_excel(file_sampling_method_i, sheet = "sampling_method")
    insect_sampling_i <- read_excel(file_sampling_method_i, sheet = "insect_sampling")

  }else{

    file_sampling_method_i <- paste(folder_base, list_files_sampling_method[i], sep = "/")
    sampling_method_i <- read_ods(file_sampling_method_i, sheet = "sampling_method")
    insect_sampling_i <- read_ods(file_sampling_method_i, sheet = "insect_sampling")

  }

  insect_sampling_methods <- insect_sampling_i$sampling_method %>% unique()
  insect_sampling_studies <- insect_sampling_i$study_id %>% unique()

  test_name_i <- paste("Right variables:", list_files_sampling_method[i], sep = " ")

  test_that(test_name_i,{

    labels_i <- labels(sampling_method_i)[[2]]
    expect_equal(labels_i, labels_OK)


  })


  test_name_i <- paste("Study identified:", list_files_sampling_method[i], sep = " ")

  test_that(test_name_i,{


    studyID_i <- any(is.na(sampling_method_i$study_id))
    expect_equal(studyID_i, FALSE)
  })


  test_name_i <- paste("type categories are OK:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(sampling_method_i$type)
    if(all(NA_values) == FALSE){
      values_pos_i <- sampling_method_i$type[!NA_values]
      expect_equal(all(values_pos_i %in% type_OK), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Studies match those in insect_sampling:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(sampling_method_i$study_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- sampling_method_i$study_id[!NA_values]
      expect_equal(all(values_pos_i %in% insect_sampling_studies), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Sampling methods match those in insect_sampling:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(sampling_method_i$sampling_method)
    if(all(NA_values) == FALSE){
      values_pos_i <- sampling_method_i$sampling_method[!NA_values]
      expect_equal(all(values_pos_i %in% insect_sampling_methods), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("sampling_richness format:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    values <- sampling_method_i$sampling_richness %in% c(0,1)
    expect_equal(all(values), TRUE)

  })

  test_name_i <- paste("sampling_abundance format:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    values <- sampling_method_i$sampling_abundance %in% c(0,1)
    expect_equal(all(values), TRUE)

  })

  test_name_i <- paste("Only ASCII characters:", list_files_sampling_method[i], sep = " ")
  test_that(test_name_i,{

    any.non.ascii <- any(grepl("I_WAS_NOT_ASCII", iconv(sampling_method_i,
                                                    "", "ASCII",
                                                    sub="I_WAS_NOT_ASCII")))


    expect_equal(any.non.ascii, FALSE)

  })

}
