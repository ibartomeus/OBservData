library(testthat)
library(tidyverse)
library(readr)
library(readxl)
library(readODS)

context("Verification that the values in the insect_sampling files with 12 variables meet our requirements")


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

folder_base <- "../Processing_files/Datasets_storage"
files_base <- list.files(folder_base)
list_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]

for (i in seq(length(list_files_insect_sampling))) {

  file_insect_sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  insect_sampling_i <- read_csv(file_insect_sampling_i,
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
                                  notes = col_character()))

  # Extract study and site information from the corresponding field_level_data
  field_level_file_name <- (strsplit(list_files_insect_sampling[i],
                         "insect_sampling_", fixed = TRUE))[[1]][2]

  field_level_file_name <- paste0("field_level_data_",field_level_file_name)
  field_level_path <- paste(folder_base, field_level_file_name, sep = "/")
  field_level_i <- read_csv(field_level_path)

  field_level_sites <- field_level_i$site_id %>% unique()
  field_level_studies <- field_level_i$study_id %>% unique()

  if(ncol(insect_sampling_i) == 12){

  field_level_sites <- field_level_i$site_id %>% unique()
  field_level_studies <- field_level_i$study_id %>% unique()

  test_name_i <- paste("Right variables:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{

    labels_i <- labels(insect_sampling_i)[[2]]
    expect_equal(labels_i, labels_OK)


  })


  test_name_i <- paste("Study identified:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{


    studyID_i <- any(is.na(insect_sampling_i$study_id))
    expect_equal(studyID_i, FALSE)
  })

  test_name_i <- paste("All sites are identified:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{

    fieldID_i <- any(is.na(insect_sampling_i$site_id))
    expect_equal(fieldID_i, FALSE)
  })


  test_name_i <- paste("abundance format:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$abundance)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$abundance[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
    expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("total_sampled_area format:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_area)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_area[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("total_sampled_time format:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_time)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_time[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("total_sampled_flowers format:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$total_sampled_flowers)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$total_sampled_flowers[!NA_values]
      expect_equal(all(0 <= values_pos_i), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })


  test_name_i <- paste("guild categories OK:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$guild)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$guild[!NA_values]
      expect_equal(all(values_pos_i %in% guids_OK), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Sites match those in field_data:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$site_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$site_id[!NA_values]
      expect_equal(all(values_pos_i %in% field_level_sites), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Studies match those in field_data:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(insect_sampling_i$study_id)
    if(all(NA_values) == FALSE){
      values_pos_i <- insect_sampling_i$study_id[!NA_values]
      expect_equal(all(values_pos_i %in% field_level_studies), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  test_name_i <- paste("Only ASCII characters:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    any.non.ascii <- any(grepl("I_WAS_NOT_ASCII", iconv(insect_sampling_i,
                                                        "", "ASCII",
                                                        sub="I_WAS_NOT_ASCII")))


    expect_equal(any.non.ascii, FALSE)

  })

}
}
