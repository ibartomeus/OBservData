
# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(sp)
library(maps)
library(maptools)
library(openxlsx)

dir_ini <- getwd()

# Load data
dir <- "Processing_files/Datasets_Processing/Blechschmidt_Ontario_Apple_2018_19/"
file <- paste0(dir,"Blechschmidt_Ontario_Apple_2018_19.xlsx")
data.site <- read.xlsx(file,
                       sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

# Extract lat-long

data.site$latitude <- parse_lat(data.site$latitude)
data.site$longitude <- parse_lon(data.site$longitude)

# Country

###############################################################################################
# FUNCTION: latlong2country
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
# SOURCE: https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
###############################################################################################

latlong2country <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('world', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)

  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

geo_data <- data.site %>%
  select(x=longitude,y=latitude)

data.site$country <- latlong2country(geo_data[,1:2])

# Fix management
data.site$management <- "conventional"


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data.site, paste0(dir,"field_level_Blechschmidt_Ontario_Apple_2018_19.csv"))
#setwd(dir_ini)


data.insect <- read.xlsx(file,
                       sheet = "field_level_data", startRow = 1)
data.insect <- as_tibble(data.insect)
