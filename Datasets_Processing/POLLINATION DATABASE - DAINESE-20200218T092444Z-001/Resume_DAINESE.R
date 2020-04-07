
library(tidyverse)

dir_ini <- getwd()
folder_base <- "C:/Users/USUARIO/Desktop/Projects/Observ/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "field_level_data"

list_files_field_level <- files_base[grepl("field_level_data", files_base)]

# extract_template_i: function that merges the dataset files


list_files_field_level <- files_base[grepl("field_level_data", files_base)]

# open file_field_level files to summarize their data


for (i in 1:length(list_files_field_level)){

  print(i)
  print(list_files_field_level[i])
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  data.site <- read_csv(file_field_level_i)
  
  study_id = unique(data.site$study_id)
  email=unique(data.site$Email_contact)
  sampling_year = as.character(unique(data.site$sampling_year))
  crop=unique(data.site$crop)
  number_sites <- nrow(data.site)
  
  if (sum(!is.na(data.site$variety))<number_sites){
    variety=paste("Missing varieties: Given ",sum(!is.na(data.site$variety))," out of ",number_sites,sep="")
  }else{
    variety="Information given"
  }
  
  if (sum(!is.na(data.site$longitude))<number_sites){
    
    if (sum(!is.na(data.site$zone_UTM))==0){
      geolocation=paste("UTM ZONE NEEDED! Missing geolocations: Given ",sum(!is.na(data.site$longitude))," out of ",number_sites,sep="")
    }else{
      geolocation=paste("Missing geolocations: ",sum(!is.na(data.site$longitude))," out of ",number_sites,sep="")
    }
    
  }else{
    geolocation="Information given"
  }
  
  
  if (sum(!is.na(data.site$pollinator_richness))<number_sites){
    richness=paste("Missing richness values: Given ",sum(!is.na(data.site$pollinator_richness))," out of ",number_sites,sep="")
  }else{
    richness="Information given"
  }
  
  if (sum(!is.na(data.site$abundance))<number_sites){
    abundance=paste("Missing abundances: Given ",sum(!is.na(data.site$abundance))," out of ",number_sites,sep="")
  }else{
    abundance="Information given"
  }

  if (sum(!is.na(data.site$visitation_rate))<number_sites){
    visitation_rate=paste("Missing visitation rates: Given ",sum(!is.na(data.site$visitation_rate))," out of ",number_sites,sep="")
  }else{
    visitation_rate="Information given"
  }

  visitation_rate_units=unique(data.site$visitation_rate_units)
  
  if (sum(!is.na(data.site$yield))<number_sites){
    yield=paste("Missing function values: Given ",sum(!is.na(data.site$yield))," out of ",number_sites,sep="")
  }else{
    yield="Information given"
  }
  
  if (sum(!is.na(data.site$yield_units))>0){
    a <- unique(data.site$yield_units)
    yield_units <- a[!is.na(a)]
  }else{
    yield_units <- NA
  }
  
  
  if (sum(!is.na(data.site$yield2_units))>0){
    a <- unique(data.site$yield2_units)
    yield2_units <- a[!is.na(a)]
    
    if (sum(!is.na(data.site$yield2))<number_sites){
      yield2=paste("Missing function values: Given ",sum(!is.na(data.site$yield2))," out of ",number_sites,sep="")
    }else{
      yield2="Information given"
    }
    
  }else{
    yield2_units <- NA
    yield2 <- NA
  }
  
  
  
  
  field_level_data <- tibble(
    study_id=study_id,
    email=email,
    number_sites=number_sites,
    sampling_year=sampling_year,
    crop=crop,
    variety=variety,
    geolocation=geolocation,
    richness=richness,
    abundance=abundance,
    visitation_rate_units=visitation_rate_units,
    visitation_rate=visitation_rate,
    yield_units=yield_units,
    yield=yield,
    yield2_units=yield2_units,
    yield2=yield2,
  )

  if (i==1){
    result <- field_level_data
  } else {
    result <- result %>% bind_rows(field_level_data) 
  }
  
}

result <- result %>% arrange(crop)

setwd(dir_ini)
write_csv(result, "DAINESE_field_level_data_RESUME.csv")

