
# This script run test on the templates in the folder "Your_new_study" and extract report on failures

report1 <- capture_output_lines({
  test_file("testthat/test-format-field_level-NEWDATA.R", reporter = "summary")
})

file.failures1 <- str_match(report1, "Failure")
f1 <- sum(!is.na(file.failures1)) #Failed test in report1

report2 <- capture_output_lines({
  test_file("testthat/test-country-NEWDATA.R", reporter = "summary")
})

file.failures2 <- str_match(report2, "Failure")
f2 <- sum(!is.na(file.failures2))#Failed test in report2

# If insect records are provided, we check their format

if (nrow(data.insect)>0){

  report3 <- capture_output_lines({
    test_file("testthat/test-format-insect_sampling-NEWDATA.R", reporter = "summary")
  })
  file.failures3 <- str_match(report3, "Failure")
  f3 <- sum(!is.na(file.failures3))#Failed test in report3

  report4 <- capture_output_lines({
    test_file("testthat/test-format-sampling-method-NEWDATA.R", reporter = "summary")
  })
  file.failures4 <- str_match(report4, "Failure")
  f4 <- sum(!is.na(file.failures4))#Failed test in report4

  if(f1+f2+f3+f4==0){
    template_OK <- TRUE
    rm(f1,f2,f3,f4,report1,report2,report3,report4,file.failures1,
       file.failures2,file.failures3,file.failures4)
  }else{
    template_OK <- FALSE
    source("Processing_files/Datasets_Processing/R-Scripts/create_test_report.R")
    rm(f1,f2,f3,f4,report1,report2,report3,report4,report_lines,file.failures1,
       file.failures2,file.failures3,file.failures4)
  }

}else{

  if(f1+f2==0){
    template_OK <- TRUE
    rm(f1,f2,report1,report2,file.failures1,file.failures2)
  }else{
    template_OK <- FALSE
    source("Processing_files/Datasets_Processing/R-Scripts/create_test_report.R")
    rm(f1,f2,report1,report2,report_lines,file.failures1,file.failures2)
  }

}
