library(testthat)
library(tidyverse)
library(readr)
library(readxl)
library(readODS)
library(tools)

context("Verification that the values in the ownership template meet our requirements")


# Test the features of datasets

# Right labels (dataframe's labels are equal to those of "ownership" template)
# Right number of columns (dataframe's number of columns is equal to that
# of "ownership" template)
# All studies are identified (study_ID is not NA)
# all study IDs are also in the field_level_data template
# role categories are in this list: "Lead author/Corresponding author","Co-author/Co-owner"
# Only ASCII characters are allowed in all columns but role.


labels_OK <- c("study_id","name","affiliation","email","role","funding")

role_OK <- c("Lead author/Corresponding author","Co-author/Co-owner")

exp_column_number <- length(labels_OK)

folder_base <- "../Your_new_study"

files <- list.files(folder_base)
excel_file <- files[grep(".xlsx",files)]
ods_file <- files[grep(".ods",files)]

if(length(excel_file)>0){
  list_files_ownsership <- excel_file
}else{
  list_files_ownsership <- ods_file
  }

for (i in seq(length(list_files_ownsership))) {

  if(length(excel_file)>0){

    file_ownership_i <- paste(folder_base, list_files_ownsership[i], sep = "/")
    ownership_i <- read_excel(file_ownership_i, sheet = "ownership")
    field_level_data_i <- read_excel(file_ownership_i, sheet = "field_level_data")

  }else{

    file_ownership_i <- paste(folder_base, list_files_ownsership[i], sep = "/")
    ownership_i <- read_ods(file_ownership_i, sheet = "ownership")
    field_level_data_i <- read_ods(file_ownership_i, sheet = "insect_sampling")

  }


  field_level_data_studies <- field_level_data_i$study_id %>% unique()

  test_name_i <- paste("Right variables:", list_files_ownsership[i], sep = " ")

  test_that(test_name_i,{

    labels_i <- labels(ownership_i)[[2]]
    expect_equal(labels_i, labels_OK)


  })


  test_name_i <- paste("Study identified:", list_files_ownsership[i], sep = " ")

  test_that(test_name_i,{


    studyID_i <- any(is.na(ownership_i$study_id))
    expect_equal(studyID_i, FALSE)
  })


  test_name_i <- paste("role categories are OK:", list_files_ownsership[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(ownership_i$role)
    if(all(NA_values) == FALSE){
      values_pos_i <- ownership_i$role[!NA_values]
      expect_equal(all(values_pos_i %in% role_OK), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Studies match those in field_level_data:", list_files_ownsership[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(ownership_i$study_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- ownership_i$study_id[!NA_values]
      expect_equal(all(values_pos_i %in% field_level_data_studies), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })



  test_name_i <- paste("Only ASCII characters (except in role):", list_files_ownsership[i], sep = " ")
  test_that(test_name_i,{

    any.non.ascii <- any(grepl("I_WAS_NOT_ASCII", iconv(ownership_i[,c(1,2,3,4,6)],
                                                    "", "ASCII",
                                                    sub="I_WAS_NOT_ASCII")))


    expect_equal(any.non.ascii, FALSE)

  })

}
