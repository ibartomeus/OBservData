
# Create automatic reports for field_level_data information

library(tidyverse)
library(sp)
library(maps)
library(maptools)

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

##########################
# FIELD RECORDS ANALYSIS
##########################

dir_ini <- getwd()

# Accessing the files in the repository

folder_base <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "field_level_data"

list_files_field_level <- files_base[grepl("field_level_data", files_base)]


for (i in 1:length(list_files_field_level)){

  print(i)
  print(list_files_field_level[i])
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  data.site.raw <- read_csv(file_field_level_i)
  
  studies <- data.site.raw %>% select(study_id,sampling_year) %>% unique()
  
  for (j in 1:nrow(studies)){
    
    if (is.na(studies$sampling_year[j])){
      data.site <- data.site.raw %>% filter(study_id==studies$study_id[j],
                                            is.na(sampling_year))
    }else{
      data.site <- data.site.raw %>% filter(study_id==studies$study_id[j],
                                            sampling_year==studies$sampling_year[j])  
    }
    
  
    study_id = unique(data.site$study_id)
    email=unique(data.site$Email_contact)
    crop=unique(data.site$crop)
    number_sites <- nrow(data.site)
    taxa_restriction <- unique(data.site$richness_restriction)
    
    if (is.na(as.character(unique(data.site$sampling_year)))){
      sampling_year="Year of sampling is missing."
    }else{
      sampling_year = as.character(unique(data.site$sampling_year))
    }
    
    if (length(taxa_restriction)>1){ #If taxa_restr. contains NA + other values
      taxa_restriction <- taxa_restriction[!is.na(taxa_restriction)]
    }
  
    if (sum(!is.na(data.site$variety))<number_sites){
      variety=paste("There are missing varieties (given ",sum(!is.na(data.site$variety))," out of ",number_sites,").",sep="")
    }else{
      variety="Full information."
    }
    
    if (sum(!is.na(data.site$longitude))<number_sites){
      
      if (sum(!is.na(data.site$zone_UTM))==0){
        geolocation=paste("UTM zone is needed. There are missing locations (given ",sum(!is.na(data.site$longitude))," out of ",number_sites,").",sep="")
      }else{
        geolocation=paste("There are missing locations (given ",sum(!is.na(data.site$longitude))," out of ",number_sites,").",sep="")
      }
      
    }else{
      geolocation="Full information."
    }
  
    #We ONLY test the sites with coordinates 
    NA_values <- is.na(data.site$latitude)
    
    if(all(NA_values) == FALSE){

        geo_data <- data.site %>% filter(!is.na(longitude),!is.na(latitude)) %>%
          select(x=longitude,y=latitude,country)
        
        expected_country_i=latlong2country(geo_data[,1:2])
        
        country_reported_i <- geo_data$country
        
        if (is.na(sum(expected_country_i==country_reported_i))|
          sum(expected_country_i==country_reported_i)<length(expected_country_i)){
          location_error <- "Reported countries may contain errors."
        }else{
          location_error <- "All reported countries are OK."
        }
          
      }else{
        location_error <- "There are no locations to check."
      }
    
    
    
    if (sum(!is.na(data.site$abundance))<number_sites){
      abundance=paste("There are missing values (given ",sum(!is.na(data.site$abundance))," out of ",number_sites,").",sep="")
    }else{
      abundance="Full information."
    }
  
    if (sum(!is.na(data.site$visitation_rate))<number_sites){
      visitation_rate=paste("There are missing values (given ",sum(!is.na(data.site$visitation_rate))," out of ",number_sites,").",sep="")
    }else{
      visitation_rate="Full information."
    }
    
    visitation_rate_units=unique(data.site$visitation_rate_units)
    
    
    if (sum(!is.na(data.site$observed_pollinator_richness))<number_sites){
      
      if ((sum(!is.na(data.site$observed_pollinator_richness))==0 & sum(!is.na(data.site$visitation_rate))>0)|
              (sum(!is.na(data.site$observed_pollinator_richness))==0 & sum(!is.na(data.site$abundance))>0)){
        richness="There is not enough taxonomic resolution to estimate richness."
      }else{
        richness=paste("There are missing values (given ",sum(!is.na(data.site$observed_pollinator_richness))," out of ",number_sites,").",sep="")
      }
      
    }else{
      richness="Full information."
    }
    
    if (is.na(taxa_restriction) & richness!="There is not enough taxonomic resolution to estimate richness."){
      taxa="No taxa restrictions have been identified."
    }else{
      taxa=taxa_restriction
    }

    
    
    # if (sum(!is.na(data.site$yield))<number_sites){
    #   yield=paste("There are missing values (given ",sum(!is.na(data.site$yield))," out of ",number_sites,").",sep="")
    # }else{
    #   yield="Full information."
    # }
    # 
    # if (sum(!is.na(data.site$yield_units))>0){
    #   a <- unique(data.site$yield_units)
    #   yield_units <- a[!is.na(a)]
    # }else{
    #   yield_units <- NA
    # }
  
    if (sum(!is.na(data.site$yield_units))>0){
      a <- unique(data.site$yield_units)
      yield_units <- a[!is.na(a)]
      
      if (sum(!is.na(data.site$yield))<number_sites){
        yield=paste("There are missing values (given ",sum(!is.na(data.site$yield))," out of ",number_sites,").",sep="")
      }else{
        yield="Full information."
      }
      
    }else{
      yield_units <- NA
      yield <- NA
    }
  
    if (sum(!is.na(data.site$yield2_units))>0){
      a <- unique(data.site$yield2_units)
      yield2_units <- a[!is.na(a)]
      
      if (sum(!is.na(data.site$yield2))<number_sites){
        yield2=paste("There are missing values (given ",sum(!is.na(data.site$yield2))," out of ",number_sites,").",sep="")
      }else{
        yield2="Full information."
      }
      
    }else{
      yield2_units <- NA
      yield2 <- NA
    }
    
  
  
  
    field_level_data <- tibble(
      `Study ID`=study_id,
      `Contact`=email,
      `Number of sites`=number_sites,
      `Year of sampling`=sampling_year,
      `Crop`=crop,
      `Variety`=variety,
      `Location`=geolocation,
      `Country`=location_error,
      `Richness`=richness,
      `Taxa constraint`=taxa,
      `Abundance`=abundance,
      `Visitation rate units`=visitation_rate_units,
      `Visitation rate`=visitation_rate,
      `Yield units`=yield_units,
      `Yield`=yield,
      `Alternative yield units`=yield2_units,
      `Alternative yield`=yield2,
    )
  
    if (i==1){
      result <- field_level_data
    } else {
      result <- result %>% bind_rows(field_level_data) 
    }
  }
}

result <- result %>% arrange(crop)


setwd(dir_ini)
write_csv(result, "Summary_field_level_data.csv")


####################
# CREATE REPORTS
####################

# Create folder
folder_for_reports <- "C:/Users/USUARIO/Desktop/Test_REPORTS"
dir.create(folder_for_reports)

for (i in 1:nrow(result)){
  file_name <- paste(folder_for_reports,result$`Study ID`[i],sep = "/")
  file_name <- paste0(file_name,".txt")
  
  headers_i <- names(result)
  
  report_lines <- c("Automatic report","")
  
  for (j in 1:ncol(result)){
    
    report_lines <- c(report_lines,
                      paste0(headers_i[j],": ",result[i,j]),"")
  
  }
  fileConn<-file(file_name)
  writeLines(report_lines, fileConn)
  close(fileConn)
  
}


