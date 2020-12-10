library(testthat)
library(tidyverse)
library(readr)

context("checks that the values in the ownership template meet our requirements")


# Test the features of datasets

# Right labels (dataframe's labels are equal to those of "ownership" template)
# All studies are identified (study_ID is not NA)
# role categories are in this list: "Lead author/Corresponding author","Co-author/Co-owner"
# Only ASCII characters are allowed in all columns but role.



labels_OK <- c("study_id","name","affiliation","email","role","funding")
role_OK <- c("Lead author/Corresponding author","Co-author/Co-owner")

exp_column_number <- length(labels_OK)

folder_base <- "../Processing_files/Datasets_storage"
files_base <- list.files(folder_base)
list_files_insect_sampling <- files_base[grepl("ownership", files_base)]



for (i in seq(length(list_files_insect_sampling))) {

  file_ownership_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  ownership_i <- read_csv(file_ownership_i,
                            col_types = cols(
                              study_id = col_character(),
                              name = col_character(),
                              affiliation = col_character(),
                              email = col_character(),
                              role = col_character(),
                              funding = col_character()))


  test_name_i <- paste("Right variables:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{

    labels_i <- labels(ownership_i)[[2]]
    expect_equal(labels_i, labels_OK)


  })


  test_name_i <- paste("Study identified:", paste0("file_",i,".csv"), sep = " ")

  test_that(test_name_i,{


    studyID_i <- any(is.na(ownership_i$study_id))
    expect_equal(studyID_i, FALSE)
  })


  test_name_i <- paste("role categories are OK:", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    NA_values <- is.na(ownership_i$role)
    if(all(NA_values) == FALSE){
      values_pos_i <- ownership_i$role[!NA_values]
      expect_equal(all(values_pos_i %in% role_OK), TRUE)

    }else{
      expect_equal(TRUE, TRUE)
    }
  })

  # test_name_i <- paste("Studies match those in field_level_data:", paste0("file_",i,".csv"), sep = " ")
  # test_that(test_name_i,{
  #
  #   NA_values <- is.na(ownership_i$study_id)
  #   if(all(NA_values) == FALSE){
  #     values_pos_i <- ownership_i$study_id[!NA_values]
  #     expect_equal(all(values_pos_i %in% field_level_data_studies), TRUE)
  #
  #   }else{
  #     expect_equal(TRUE, TRUE)
  #   }
  # })



  test_name_i <- paste("Only ASCII characters (except in role):", paste0("file_",i,".csv"), sep = " ")
  test_that(test_name_i,{

    any.non.ascii <- any(grepl("I_WAS_NOT_ASCII", iconv(ownership_i[,c(1,2,3,4,6)],
                                                        "", "ASCII",
                                                        sub="I_WAS_NOT_ASCII")))


    expect_equal(any.non.ascii, FALSE)

  })

}
