library(testthat)
library(dplyr)
library(sp)
library(maps)
library(maptools)

context("checks that latitude and longitude match country value")

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


folder_base <- "../Datasets_storage"
files_base <- list.files(folder_base)
list_files_field_level <- files_base[grepl("field_level_data", files_base)]

for (i in seq(length(list_files_field_level))) {

  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  field_level_i <- read.csv(file_field_level_i, 
                            stringsAsFactors = F)
  
  field_level_i <- as_tibble(field_level_i)
  
  #We ONLY test the sites with coordinates 
  NA_values <- is.na(field_level_i$latitude)
  
  if(all(NA_values) == FALSE){
    
    test_name_i <- paste("Right country:", list_files_field_level[i], sep = " ")
  
    test_that(test_name_i,{
      
      geo_data <- field_level_i %>% filter(!is.na(longitude),!is.na(latitude),) %>%
        select(x=longitude,y=latitude,country)
      
      expected_country_i=latlong2country(geo_data[,1:2])
      
      country_i <- geo_data$country
      
      expect_equal(expected_country_i, country_i)
    })
    
  }

}