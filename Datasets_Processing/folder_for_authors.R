
library(tidyverse)

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
  
  #Some studies did not provide insect sampling data. We check if such file exists
    
  study_code <- str_split(list_files_field_level[i], "field_level_data_")
  insect_sampling_i <- paste0("insect_sampling_", study_code[[1]][2])
  code <- str_split(study_code[[1]][2], ".csv")
  
  file_insect_sampling_i <- paste(folder_base, insect_sampling_i, sep = "/")
  
  if(file.exists(file_insect_sampling_i)==T){
    file_insect_sampling_i <- insect_sampling_i
  }else{
    file_insect_sampling_i <- NA
  }
  
  
  field_level_data <- tibble(
    code=code[[1]][1],
    file_field=list_files_field_level[i],
    file_insect=file_insect_sampling_i,
    Credit=unique(data.site$Credit)
  )
    
  if (i==1){
    result <- field_level_data
  } else {
    result <- result %>% bind_rows(field_level_data) 
  }  

  
}

result$author_folder <- NA

result %>% filter(grepl("(UC Berkeley)",result$Credit,ignore.case = TRUE)) 
result %>% filter(is.na(result$author_folder)) 


result$author_folder[grepl("Rachael",result$Credit,ignore.case = TRUE)] <- "Rachael Winfree"
result$author_folder[grepl("Bomm",result$Credit,ignore.case = TRUE)] <- "Riccardo Bommarco"
result$author_folder[grepl("Alexandra-Maria Klein",result$Credit,ignore.case = TRUE)] <- "Alexandra-Maria Klein"

result$author_folder[grepl("Ignasi Bartomeus",result$Credit,ignore.case = TRUE)] <- "Ignasi Bartomeus"
result$author_folder[grepl("Mandelik",result$Credit,ignore.case = TRUE)] <- "Yael Mandelik"
result$author_folder[grepl("Garr",result$Credit,ignore.case = TRUE)] <- "Michael Garratt"
result$author_folder[grepl("Kenna E. Mackenzie",result$Credit,ignore.case = TRUE)] <- "David Kleijn"
result$author_folder[grepl("Veldtman",result$Credit,ignore.case = TRUE)] <- "Ruan Veldtman"
result$author_folder[grepl("Brand",result$Credit,ignore.case = TRUE)] <- "Ruan Veldtman"
result$author_folder[grepl("Brand",result$Credit,ignore.case = TRUE)] <- "Ruan Veldtman"

result$author_folder[grepl("Ricketts",result$Credit,ignore.case = TRUE)] <- "Taylor Ricketts"
result$author_folder[grepl("may",result$Credit,ignore.case = TRUE)] <- "Margaret Mayfield"
result$author_folder[grepl("blande",result$Credit,ignore.case = TRUE)] <- "Blande Viana"
result$author_folder[grepl("shalene",result$Credit,ignore.case = TRUE)] <-"Shalene Jha"

result$author_folder[grepl("Burn",result$Credit,ignore.case = TRUE)] <-"Katherine LW Burns"
result$author_folder[grepl("Georg",result$Credit,ignore.case = TRUE)] <-"Georg Andersson"
result$author_folder[grepl("Boreux",result$Credit,ignore.case = TRUE)] <-"Virginie Boreux"
result$author_folder[grepl("Freit",result$Credit,ignore.case = TRUE)] <-"Breno M. Freitas"
result$author_folder[grepl("Carvalheiro",result$Credit,ignore.case = TRUE)] <-"Luísa G. Carvalheiro"
result$author_folder[grepl("Pablo Cavigliasso",result$Credit,ignore.case = TRUE)] <-"Pablo Cavigliasso"
result$author_folder[grepl("Natacha Chacoff",result$Credit,ignore.case = TRUE)] <-"Natacha Chacoff"
result$author_folder[grepl("Alice Claßen",result$Credit,ignore.case = TRUE)] <-"Alice Claßen"
result$author_folder[grepl("Saul A. Cunningham",result$Credit,ignore.case = TRUE)] <-"Saul A. Cunningham"
result$author_folder[grepl("Johan Ekroos",result$Credit,ignore.case = TRUE)] <-"Johan Ekroos"
result$author_folder[grepl("Heather Grab",result$Credit,ignore.case = TRUE)] <-"Heather Lee Grab"
result$author_folder[grepl("Violeta Hevia",result$Credit,ignore.case = TRUE)] <-"Violeta Hevia"
result$author_folder[grepl("Juliana Hipólito",result$Credit,ignore.case = TRUE)] <-"Juliana Hipólito"
result$author_folder[grepl("Jessica D. Petersen",result$Credit,ignore.case = TRUE)] <-"Jessica D. Petersen"
result$author_folder[grepl("knapp",result$Credit,ignore.case = TRUE)] <-"Jessica Knapp"
result$author_folder[grepl("Smitha Krishnan",result$Credit,ignore.case = TRUE)] <-"Smitha Krishnan"
result$author_folder[grepl("David Crowder",result$Credit,ignore.case = TRUE)] <-"David Crowder"
result$author_folder[grepl("Fabiana Oliveira",result$Credit,ignore.case = TRUE)] <-"Fabiana Oliveira da Silva"
result$author_folder[grepl("Mark Otieno",result$Credit,ignore.case = TRUE)] <-"Mark Otieno"
result$author_folder[grepl("Ana Montero-Castaño",result$Credit,ignore.case = TRUE)] <-"Ana Montero-Castaño"
result$author_folder[grepl("Simon Potts",result$Credit,ignore.case = TRUE)] <-"Simon Potts"
result$author_folder[grepl("Romina Rader",result$Credit,ignore.case = TRUE)] <-"Romina Rader"
result$author_folder[grepl("Agustin Saez",result$Credit,ignore.case = TRUE)] <-"Agustin Saez"
result$author_folder[grepl("Jeroen Scheper",result$Credit,ignore.case = TRUE)] <-"Jeroen Scheper"
result$author_folder[grepl("Christof Schuepp",result$Credit,ignore.case = TRUE)] <-"Christof Schüepps"
result$author_folder[grepl("(UC Berkeley)",result$Credit,ignore.case = TRUE)] <-"Claire Kremen"
result$author_folder[grepl("Dara Stanley",result$Credit,ignore.case = TRUE)] <-"Dara Stanley"
result$author_folder[grepl("Louis Sutter",result$Credit,ignore.case = TRUE)] <-"Louis Sutter"
result$author_folder[grepl("Hisatomo Taki",result$Credit,ignore.case = TRUE)] <-"Hisatomo Taki"
result$author_folder[grepl("jens",result$Credit,ignore.case = TRUE)] <-"Jens Åström"
result$author_folder[grepl("Carlos H. Vergara",result$Credit,ignore.case = TRUE)] <-"Carlos H. Vergara"
result$author_folder[grepl("Bryony Willcox",result$Credit,ignore.case = TRUE)] <-"Bryony Willcox"
result$author_folder[grepl("zou",result$file_insect,ignore.case = TRUE)] <-"Yi Zou"
result$author_folder[grepl("Betina Blochtein",result$Credit,ignore.case = TRUE)] <-"Betina Blochtein"
result$author_folder[grepl("de groo",result$Credit,ignore.case = TRUE)] <-"David Kleijn"
result$author_folder[grepl("Nicolas J. Vereecken",result$Credit,ignore.case = TRUE)] <-"Nicolas J. Vereecken"
result$author_folder[grepl("Davi L. Ramos",result$Credit,ignore.case = TRUE)] <-"WITHOUT OK Davi L. Ramos"
result$author_folder[grepl("https://snd.gu.se",result$Credit,ignore.case = TRUE)] <-"WITHOUT OK Rebecca Steward"
result$author_folder[grepl("Rachel Mallinger",result$Credit,ignore.case = TRUE)] <-"WITHOUT OK Rachel Mallinger"
result$author_folder[grepl("Sarah Cusser",result$Credit,ignore.case = TRUE)] <-"WITHOUT OK Sarah Cusser"
result$author_folder[is.na(result$author_folder)] <- result$Credit[is.na(result$author_folder)]

write_csv(result,"files_list.csv")

