library(testthat)
library(tidyverse)
library(readr)

context("checks that the values in the insect_sampling template meet our requirements")


# Test the features of datasets

# Right labels (dataframe's labels are equal to those of "insect_sampling" template)
# Right number of columns (dataframe's number of columns is equal to that of "insect_sampling" template)
# All studies are identified (study_ID is not NA)
# All sites are identified (site_ID is not NA)
# abundance format  (non-negative number or NA)
# total_sampled_area format  (non-negative number or NA)
# total_sampled_time format  (non-negative number or NA)
# total_sampled_flowers format  (non-negative number or NA)


labels_OK <- c("study_id","site_id","pollinator","guild","sampling_method",
               "abundance","total_sampled_area","total_sampled_time",
               "total_sampled_flowers","Description")       


exp_column_number <- length(labels_OK)

folder_base <- "../Datasets_storage"
files_base <- list.files(folder_base)
list_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]

for (i in seq(length(list_files_insect_sampling))) {
  
  file_insect_sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  insect_sampling_i <- read_csv(file_insect_sampling_i,
                            col_types = cols(
                              study_id = col_character(),
                              site_id = col_character(),
                              pollinator = col_character(),
                              guild = col_character(),
                              sampling_method = col_character(),
                              abundance = col_double(),
                              total_sampled_area = col_double(),
                              total_sampled_time = col_double(),
                              total_sampled_flowers = col_double(),
                              Description = col_character()))
  
  test_name_i <- paste("Right labels:", list_files_insect_sampling[i], sep = " ")
  
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
  
  

}
