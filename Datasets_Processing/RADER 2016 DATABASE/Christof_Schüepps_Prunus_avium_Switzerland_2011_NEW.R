
library(tidyverse)
library(sp) #Transforming latitude and longitude
library(maps)
library(maptools)

dir_ini <- getwd()

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


################
# LOAD DATA
################

field_level_data <- read_csv("Individual CSV/field_level_data_Christof_Schüepps_Prunus_avium_Switzerland_2011_expanded.csv")

insect_sampling <- read_csv("Individual CSV/insect_sampling_Christof_Schüepps_Prunus_avium_Switzerland_2011_expandedd.csv")

#################
# INSECT SAMPLING
#################

# Fix insect sampling flowers

insect_sampling$total_sampled_flowers[insect_sampling$total_sampled_flowers==2333333333]=2.333333333
insect_sampling$total_sampled_flowers[insect_sampling$total_sampled_flowers==2666666667]=2.666666667
insect_sampling$total_sampled_flowers[insect_sampling$total_sampled_flowers==3333333333]=3.333333333
insect_sampling$total_sampled_flowers[insect_sampling$total_sampled_flowers==1666666667]=1.666666667

sampled_flowers <- field_level_data %>% select(site_id,total_sampled_area) %>%
  rename(fower_info=total_sampled_area)


insect_sampling_new <- insect_sampling %>% 
  left_join(sampled_flowers,by="site_id") %>%
  mutate(Description=paste(fower_info,". ",Description)) %>%
  select(-fower_info)

insect_sampling_new$Description[insect_sampling_new$Description=="NA .  NA"] <- NA

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_new, "insect_sampling_Christof_Schüepps_Prunus_avium_Switzerland_2011.csv")
setwd(dir_ini)




library(rgdal)
d <- field_level_data %>% select(longitude, latitude)
coordinates(d) <- c("latitude","longitude")
proj4string(d) <- CRS("+init=epsg:21781") # LV95, EPSG=2056

d@coords


d.new <- spTransform(d, CRS("+init=epsg:4326")) #WGS84
d.new2 <- spTransform(d, CRS("+proj=longlat +datum=WGS84"))
plot(d)
plot(d.new)
plot(d.new2)


new_coords <- (d.new@coords)
field_level_data$longitude <-  new_coords[1:28]
field_level_data$latitude <- new_coords[29:56]

latlong2country(d.new@coords)

latlong2country(data.frame(x=new_coords[1:28],y=new_coords[29:56]))

field_level_data$sampling_start_month <- 4
field_level_data$sampling_end_month <- 5

field_level_data$total_sampled_area <- NA

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Christof_Schüepps_Prunus_avium_Switzerland_2011.csv")
setwd(dir_ini)
