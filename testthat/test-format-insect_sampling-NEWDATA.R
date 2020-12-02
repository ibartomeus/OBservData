library(testthat)
library(tidyverse)
library(readr)
library(readxl)
library(readODS)

context("Verification that the values in the insect_sampling template meet our requirements")


# Test the features of datasets

# Right labels (dataframe's labels are equal to those of "insect_sampling" template)
# Right number of columns (dataframe's number of columns is equal to that of "insect_sampling" template)
# All studies are identified (study_ID is not NA)
# All sites are identified (site_ID is not NA)
# abundance format  (non-negative number or NA)
# total_sampled_area format  (non-negative number or NA)
# total_sampled_time format  (non-negative number or NA)
# total_sampled_flowers format  (non-negative number or NA)
# guild categories are in this list: "honeybees","bumblebees",
# "other_wild_bees", "syrphids","humbleflies","other_flies","beetles",
# "non_bee_hymenoptera","lepidoptera","other"
# all study IDs are also in field_level template
# all site IDs are also in field_level template
# Only ASCII characters are allowed.

labels_OK <- c("study_id","site_id","sampling_method",
               "pollinator","identified_to","guild",
               "abundance","total_sampled_area","total_sampled_time",
               "total_sampled_flowers","description","notes")

guids_OK <- c("honeybees","bumblebees","other_wild_bees",
              "syrphids","humbleflies","other_flies",
              "beetles","non_bee_hymenoptera","lepidoptera",
              "other")

exp_column_number <- length(labels_OK)

folder_base <- "../Your_new_study"

files <- list.files(folder_base)
excel_file <- files[grep(".xlsx",files)]
ods_file <- files[grep(".ods",files)]

if(length(excel_file)>0){
  list_files_insect_sampling <- excel_file
}else{
  list_files_insect_sampling <- ods_file
  }

for (i in seq(length(list_files_insect_sampling))) {

  if(length(excel_file)>0){

    file_insect_sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
    insect_sampling_i <- read_excel(file_insect_sampling_i, sheet = "insect_sampling")
    field_level_i <- read_excel(file_insect_sampling_i, sheet = "field_level_data")

  }else{

    file_insect_sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
    insect_sampling_i <- read_ods(file_insect_sampling_i, sheet = "insect_sampling")
    field_level_i <- read_ods(file_insect_sampling_i, sheet = "field_level_data")

  }

  field_level_sites <- field_level_i$site_id %>% unique()
  field_level_studies <- field_level_i$study_id %>% unique()

  test_name_i <- paste("Right variables:", list_files_insect_sampling[i], sep = " ")

  test_that(test_name_i,{

    labels_i <- labels(insect_sampling_i)[[2]]
    expect_equal(labels_i, labels_OK)


  })


  test_name_i <- paste("Study identified:", list_files_insect_sampling[i], sep = " ")

  test_that(test_name_i,{


    studyID_i <- any(is.na(insect_sampling_i$study_id))
    expect_equal(studyID_i, FALSE)
  })

  test_name_i <- paste("All sites are identified:", list_files_insect_sampling[i], sep = " ")

  test_that(test_name_i,{

    fieldID_i <- any(is.na(insect_sampling_i$site_id))
    expect_equal(fieldID_i, FALSE)
  })


  test_name_i <- paste("abundance format:", list_files_insect_sampling[i], sep = " ")

  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$abundance)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$abundance[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
    expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("total_sampled_area format:", list_files_insect_sampling[i], sep = " ")

  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_area)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_area[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("total_sampled_time format:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_time)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_time[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("total_sampled_flowers format:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_flowers)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_flowers[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("guild categories OK:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$guild)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$guild[!NA_values]
      expect_equal(all(values_pos_i %in% guids_OK), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Sites match those in field_data:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$site_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$site_id[!NA_values]
      expect_equal(all(values_pos_i %in% field_level_sites), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Studies match those in field_data:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$study_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$study_id[!NA_values]
      expect_equal(all(values_pos_i %in% field_level_studies), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Only ASCII characters:", list_files_insect_sampling[i], sep = " ")
  test_that(test_name_i,{

    any.non.ascii <- any(grepl("I_WAS_NOT_ASCII", iconv(insect_sampling_i,
                                                        "", "ASCII",
                                                        sub="I_WAS_NOT_ASCII")))


    expect_equal(any.non.ascii, FALSE)

  })

}
