
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
# Load feedback from Rachel
##########################

field_level_data <- read_csv("DATASETS/field 2013 NEW.csv")
insect_sampling <- read_csv("DATASETS/IS 2013.csv")

# Update credit and contact info
field_level_data$Credit <- "Rachel Mallinger, University of Florida" 
field_level_data$Email_contact <- "rachel.mallinger@ufl.edu"


UTM_16 <- field_level_data %>% filter(zone_UTM==16) %>% select(X_UTM,Y_UTM)

sputm <- SpatialPoints(UTM_16[,1:2], proj4string=CRS("+proj=utm +zone=16 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

UTM_16$longitude <- NA
UTM_16$latitude <- NA
UTM_16[,3:4] <- spgeo@coords

UTM_15 <- field_level_data %>% filter(zone_UTM==15) %>% select(X_UTM,Y_UTM)

sputm <- SpatialPoints(UTM_15[,1:2], proj4string=CRS("+proj=utm +zone=15 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

UTM_15$longitude <- NA
UTM_15$latitude <- NA
UTM_15[,3:4] <- spgeo@coords



setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Rachel_Mallinger_Malus_domestica_USA_2013.csv")
setwd(dir_ini)
                               

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Rachel_Mallinger_Malus_domestica_USA_2013.csv")
setwd(dir_ini)

