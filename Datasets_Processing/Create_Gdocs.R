
library(tidyverse)

# get current working directory
dir_ini <- getwd()

data.folders <- read_csv("files_list_FOLDERS.csv")

data.folders$Credit <- str_replace_all(data.folders$Credit," and ",", ") 
data.folders$Credit <- str_replace_all(data.folders$Credit,",,",",")
data.folders$Credit <- str_replace_all(data.folders$Credit," & ",", ")
data.folders$Credit <- str_replace_all(data.folders$Credit," et al.",", co-authors")
data.folders$Credit <- str_replace_all(data.folders$Credit,";",",")

data.folders$Credit <- str_replace_all(data.folders$Credit,"Heather Grab, USDA NASS","Heather Grab (USDA NASS)")

data.folders$Credit <- str_replace_all(data.folders$Credit,"Knapp, co-authors 19","Jessica Knapp, Rosalind F.Shaw, Juliet L.Osborne")
data.folders$Credit <- str_replace_all(data.folders$Credit,"Smitha Krishnan, ETH Zurich","Smitha Krishnan (ETH Zurich)")
data.folders$Credit <- str_replace_all(data.folders$Credit,"Rachel Mallinger, USDA-ARS","Rachel Mallinger (USDA-ARS)")
data.folders$Credit <- str_replace_all(data.folders$Credit,"Hisatomo Taki, Forestry, forest products research institute",
                                       "Hisatomo Taki (Forestry, forest products research institute)")

data.folders$Credit <- str_replace_all(data.folders$Credit,"Mariëtte R. Brand","Mariëtte R. Brand, Ruan Veldtman")
data.folders$Credit <- str_replace_all(data.folders$Credit,". Landscape Data:",",")

data.folders$Credit[data.folders$Credit=="G.A. (Arjen) de Groot, Wageningen Environmental Research (Alterra) + David Kleijn, Wageningen University"] <- 
  "G.A. (Arjen) de Groot (Wageningen Environmental Research, Alterra), David Kleijn (Wageningen University)"

data.folders$Credit[data.folders$Credit=="Agustin Saez/CONICET (Universidad Nacional del Comahue)"] <- 
  "Agustin Saez (CONICET, Universidad Nacional del Comahue)"

folder_procc_data <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_storage"

# List of authors

authors <- unique(data.folders$author_folder)

for (i in 1:length(authors)){

  # List studies for author_i
  
  studies <- data.folders %>% filter(author_folder==authors[i])
  
  
  for (j in 1:nrow(studies)){
    
  study_i <- studies$code[j]
  
  authors_i <- str_split(studies$Credit[j],", ")[[1]]
  
  study_i_data <- tibble(`Study ID`=study_i,Name=authors_i,Affiliation=NA,email=NA,Role=NA)
  
  # fill email and leading author
  csv_file_field_location <- paste(folder_procc_data,studies$file_field[j],sep = "/")
  data.site <- read_csv(csv_file_field_location)
  
  Lead_i <- studies$author_folder[j]
  study_i_data$email[study_i_data$Name==Lead_i] <- unique(data.site$Email_contact)
  study_i_data$Role[study_i_data$Name==Lead_i] <- "Lead author/Corresponding author"
  study_i_data$Role[is.na(study_i_data$Role)] <- "Co-author"
  
  if(i==1 & j==1){
    final_Gdocs <- study_i_data
  }else{
    final_Gdocs <- bind_rows(final_Gdocs,study_i_data)
  }
  
  }
}

write_csv(final_Gdocs,"final_Gdocs.csv")
