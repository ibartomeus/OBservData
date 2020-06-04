
library(tidyverse)
library(mapr)
library(tmaptools)
library(sf)
library(tmap)


dir_ini <- getwd()
folder_base <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "field_level_data"

list_files_field_level <- files_base[grepl("field_level_data", files_base)]


for (i in 1:length(list_files_field_level)){

  print(i)
  print(list_files_field_level[i])
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  data.site <- read_csv(file_field_level_i)
  
  if(list_files_field_level[i]!="field_level_data_vere01.csv"){
  
  field_level_data <- tibble(
    study_id=unique(data.site$study_id),
    crop = unique(data.site$crop),
    year = as.character(unique(data.site$sampling_year)),
    email=unique(data.site$Email_contact),
    number_sites=nrow(data.site),
    latitude = mean(data.site$latitude,na.rm = T),
    longitude = mean(data.site$longitude,na.rm = T)

  )
  
  if (i==1){
    result <- field_level_data
  } else {
    result <- result %>% bind_rows(field_level_data) 
  }
  }else{
    subsys <- unique(data.site$study_id)
    for (j in 1:length(subsys)){
      
      data.site_j <- data.site %>% filter(data.site$study_id==subsys[j])
      
      field_level_data <- tibble(
        study_id=unique(data.site_j$study_id),
        crop = unique(data.site_j$crop),
        year = as.character(unique(data.site$sampling_year)),
        email=unique(data.site_j$Email_contact),
        number_sites=nrow(data.site_j),
        latitude = mean(data.site_j$latitude,na.rm = T),
        longitude = mean(data.site_j$longitude,na.rm = T)
      )
      
      if (i==1){
        result <- field_level_data
      } else {
        result <- result %>% bind_rows(field_level_data) 
      }
        
    }
    
    
    
  }

  
}

result <- result %>% arrange(crop)



result_geo <- result %>% filter(!is.nan(latitude))

result_nan <- result %>% filter(is.nan(latitude))
result_nan$place <- c("New Zealand, North Island",
           "France, Loire",
           "Italy, South Italy",
           "Brazil, Ceará",
           "Brazil, Ceará",
           "Brazil",
           "Brazil",
           "Brazil",
           "Sweden, Uppsala",
           "Sweden, Västergötland",
           "UK, Kent",
           "Sweden, Scandinavia",
           "Sweden, Scandinavia",
           "Sweden, Scania",
           "Sweden, Scania",
           "Switzerland, Zurich",
           "Kenya, Kibwezi",
           "Israel",
           "Costa Rica, Perez Zeledon",
           "Costa Rica, Perez Zeledon",
           "Brazil, Chapada Diamantina",
           "Brazil, Chapada Diamantina",
           "USA, New York",
           "Poland",
           "UK, Yorkshire",
           "Brazil, Ceará",
           "Brazil, Ceará",
           "Brazil, Ceará",
           "USA, Wisconsin",
           "USA, Wisconsin",
           "UK, Kent",
           "Brazil, Bahia",
           "Switzerland",
           "USA, California",
           "USA, Sacramento Valley",
           "USA, California",
           "Sweden, Östergötland",
           "Sweden, Skåne",
           "Sweden, Östergötland",
           "Argentina, Ñandubay",
           "UK, Berkshire"
)

for(i in 1:length(result_nan$place)){
  temp <- geocode_OSM(result_nan$place[i])
  result_nan$longitude[i] <- temp$coords[1]
  result_nan$latitude[i] <- temp$coords[2]
}

result_mod <- bind_rows(result_geo,result_nan) %>% select(study_id,crop,year,email,
                                                      number_sites,latitude,longitude,place) %>%
  arrange(crop)

setwd(dir_ini)
write_csv(result_mod, "Summary_field_level_data_Nacho.csv")
