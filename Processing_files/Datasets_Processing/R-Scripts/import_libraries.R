
if (!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)# to handle data
}

if (!require(testthat)){
  install.packages('testthat')
  library(testthat)# to run tests
}

if (!require(sp)){
  install.packages('sp')
  library(sp) # to check location
}

if (!require(maps)){
  install.packages('maps')
  library(maps)# to check location
}

if (!require(maptools)){
  install.packages('maptools')
  library(maptools)# to check location
}

if (!require(iNEXT)){
  install.packages('iNEXT')
  library("iNEXT") # to estimate Chao1
}

if (!require(readxl)){
  install.packages('readxl')
  library(readxl) #to read xlsx files
}

if (!require(readODS)){
  install.packages('readODS')
  library(readODS) # to read ods files
}

if (!require(rmarkdown)){
  install.packages('rmarkdown')
  library(rmarkdown) # to create pdf reports
}

if (!require(tools)){
  install.packages('tools')
  library(tools) # to identify non-ASCII characters
}
