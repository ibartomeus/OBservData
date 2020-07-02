
library(tidyverse)
library(sp)
library(maps)
library(maptools)
source("Create_field_reports.R")
source("Create_general_report2.R")
source("Create_excel_authors.R")
library(openxlsx)
library(xlsx)
library(rmarkdown)

# get current working directory
dir_ini <- getwd()

data.folders.old <- read_csv("files_list_FOLDERS.csv") %>% rename(old_code=code)
data.folders.old$Location[data.folders.old$old_code=="Bart01"] <- "Ignasi_Bartomeus_Brassica_napus_Sweden_2013"
data.folders.old$Location[data.folders.old$old_code=="Burn01"] <- "Katherine_LW_Burns_Malus_domestica_Ireland_2018"
data.folders.old$Location[data.folders.old$old_code=="hevi01"] <- "Violeta_Hevia_Helianthus_annuus_Spain_2017"
data.folders.old$Location[data.folders.old$old_code=="knap01"] <- "Jessica_Knapp_Cucurbita_pepo_UK_2016"
data.folders.old$Location[data.folders.old$old_code=="Lichtenberg_Crowder_canola"] <- "David_Crowder_Brassica_napus_USA_several_years"
data.folders.old$Location[data.folders.old$old_code=="mont01"] <- "Ana_Montero_CastaÃ±o_Vaccinium_corymbosum_Canada_2018"
data.folders.old$Location[data.folders.old$old_code=="vere01"] <- "Nicolas_J_Vereecken_several_crops_several_countries_several_years"
data.folders.old$Location[data.folders.old$old_code=="Szen01"] <- "Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005"


data.new.names <- read_csv("new_names_studies.csv") %>% rename(code=new_code)

data.folders <- data.new.names %>% mutate(file_field=paste0("field_level_data_",code,".csv"),
                                         file_insect=paste0("insect_sampling_",code,".csv")) %>%
  left_join(data.folders.old[,c(1,4:8)],by="old_code")

folder_for_authors <- "C:/Users/USUARIO/Desktop/Test_FOLDER"
dir.create(folder_for_authors)

folder_raw_data <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_Processing"
folder_procc_data <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_storage"
folder_thesaurus <- "C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators"

# Excel for authors
excel_authors <- openxlsx::read.xlsx("Authors_Gdocs.xlsx", startRow = 1) %>%
  rename(`Study ID`=Study.ID,
         `I checked the processed data and it is correct`=`I.checked&#10;the.processed&#10;data.and&#10;it.is.correct.`,
         `I confirm all coauthors are added and affiliations are correct`=`I.confirm.all&#10;coauthors&#10;are.added.and.&#10;affiliations.are.&#10;correct.`)


# List of authors

authors <- unique(data.folders$author_folder)

for (i in 1:length(authors)){
  
  # Create folder for author_i
  
  base_author_i <- paste(folder_for_authors,authors[i],sep="/")
  dir.create(base_author_i)
  
  
  # List studies for author_i
  
  studies <- data.folders %>% filter(author_folder==authors[i])
  
  
  ######################
  # CREATE Data ownership template
  ######################
  
  # Open xlsx for authors
  
  create_excel_authors(authors,i,studies,base_author_i,excel_authors)
  
  
  ######################
  # CREATE GENERAL REPORT
  ######################
  
  create_general_report(authors,i,studies,base_author_i,folder_procc_data)
  
  
  for (j in 1:nrow(studies)){
    
  study_i <- studies$code[j]
  base_study_i <- paste(base_author_i,study_i,sep="/")
  
  # Create base_study_i folder
  
  dir.create(base_study_i)
  
  ##################
  # Create background information on data file conversion folder
  ##################
  
  # Create raw data folder
  background_data_folder <- paste(base_study_i,"Background information on data file conversion",sep="/")
  dir.create(background_data_folder)
  
  ##################
  # Copy R file
  ##################
  
  R_file_location <- paste(folder_raw_data,studies$Location[j],sep = "/")
  R_file <- paste0(R_file_location,"/",studies$code[j],".R")
  new_R_file <- paste0(background_data_folder,"/",studies$code[j],".R")
  
  file.copy(from=R_file, to = new_R_file, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  ##################
  # Copy metadata file
  ##################
  
  meta_file <- "C:/Users/USUARIO/Desktop/OBservData/Template/Metadata_V6p4.xlsx"
  new_meta_file <- paste0(base_study_i,"/Explanation column names.xlsx")
  
  file.copy(from=meta_file, to = new_meta_file, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  #############################################################
  # Copy additional R file to add taxon constraints (if needed)
  #############################################################
  
  if(
    (!studies$Location[j] %in% c("KLEIJN 2015 DATABASE",
                                  "GARIBALDI 2015 DATABASE",
                                  "GARIBALDI 2016 DATABASE")) &
     (!authors[i] %in% c("Alejandro Trillo","Marcos Miñarro","Amparo Lázaro","Katrine Hansen"))
     ){
    
    additional_R_file <- paste0(folder_raw_data,"/","add_taxon_constraint_column_DAINESE_RADER_OTHER.R")
    new_additional_R_file <- paste0(background_data_folder,"/","add_taxon_constraint_column_DAINESE_RADER_OTHER.R")
    file.copy(from=additional_R_file, to = new_additional_R_file, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
  
  #############################################################
  # Copy additional taxon table for RADER
  #############################################################
  
  if(studies$Location[j] == "RADER 2016 DATABASE"){
    
    additional_csv_file <- paste0(folder_raw_data,"/RADER 2016 DATABASE/","taxon_table_Rader.csv")
    new_additional_csv_file <- paste0(background_data_folder,"/","taxon_table_Rader.csv")
    file.copy(from=additional_csv_file, to = new_additional_csv_file, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
  
  ##################
  # Copy Guild table
  ##################
  
  if(!authors[i] %in% c("Alejandro Trillo","Marcos Miñarro","Amparo Lázaro","Katrine Hansen")){
  
  thesaurus_file <- paste(folder_thesaurus,"Table_organism_guild_META.csv",sep = "/")
  new_thesaurus_file <- paste0(background_data_folder,"/","Table_organism_guild_META.csv")
  
  file.copy(from=thesaurus_file, to = new_thesaurus_file, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  }
  
  #######################
  # Copy resulting csv's
  #######################
  
  csv_file_field_location <- paste(folder_procc_data,studies$file_field[j],sep = "/")
  new_csv_file_field_location <- paste(base_study_i,studies$file_field[j],sep = "/")
  
  if (!is.na(studies$file_insect[j])){
    csv_file_insect_location <- paste(folder_procc_data,studies$file_insect[j],sep = "/")
    csv_resulting_files <- c(csv_file_field_location,csv_file_insect_location)
    
    new_csv_file_insect_location <- paste(base_study_i,studies$file_insect[j],sep = "/")
    new_csv_resulting_files <- c(new_csv_file_field_location,new_csv_file_insect_location)
    
  }else{
    csv_resulting_files <- c(csv_file_field_location)
    new_csv_resulting_files <- c(new_csv_file_field_location)
  }
  
  file.copy(from=csv_resulting_files, to = new_csv_resulting_files, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  #################
  # Create Report
  #################
  
  create_report(base_study_i,base_study_i,studies$Additional_comments[j],studies$Location[j])
  
  #################
  # Copy raw data
  #################
  
  # Create raw data folder
  raw_data_folder <- paste(base_study_i,"Raw Data",sep="/")
  dir.create(raw_data_folder)
  
  # Copy raw data files to raw data folder
  
  if (studies$Location[j]=="KLEIJN 2015 DATABASE"){
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],studies$Raw_data_Folder[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    new_list_of_files <- paste0(raw_data_folder,"/",list.files(original_raw_data_folder))
    
    
    # Copy files 
    
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    
  }else if(studies$Location[j]=="GARIBALDI 2015 DATABASE"){
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],studies$Raw_data_Folder[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    
    # Select files for a given code
    
    list_of_files <- list_of_files[grepl(studies$old_code[j],list_of_files,ignore.case = TRUE)]
    
    # Create new files list
    
    new_list_of_files <- list.files(original_raw_data_folder)[grepl(studies$old_code[j],list.files(original_raw_data_folder),ignore.case = TRUE)]
    new_list_of_files <- paste0(raw_data_folder,"/",new_list_of_files)
    
    #Copy files
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    
  }else if(studies$Location[j]=="GARIBALDI 2016 DATABASE"){
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],studies$Raw_data_Folder[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    new_list_of_files <- paste0(raw_data_folder,"/",list.files(original_raw_data_folder))
    
    # Copy files 
    
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    
  }else if(studies$Location[j]=="RADER 2016 DATABASE"){
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],studies$Raw_data_Folder[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    #new_list_of_files <- paste0(raw_data_folder,"/",list.files(original_raw_data_folder))
    
    # Select files for a given code
    
    if(studies$code[j]=="Jauker_2006"){
      list_of_files <- list_of_files[grepl("Jauker",list_of_files,ignore.case = TRUE)]
      new_list_of_files <- list.files(original_raw_data_folder)[grepl("Jauker",list.files(original_raw_data_folder),ignore.case = TRUE)]
      new_list_of_files <- paste0(raw_data_folder,"/",new_list_of_files)
      #new_list_of_files <- new_list_of_files[grepl("Jauker",new_list_of_files,ignore.case = TRUE)]
    }
    
    else if(studies$code[j]=="Schueep_NA"){
      list_of_files <- list_of_files[grepl(studies$old_code[j],list_of_files,ignore.case = TRUE)]
      
      new_list_of_files <- list.files(original_raw_data_folder)[grepl(studies$old_code[j],list.files(original_raw_data_folder),ignore.case = TRUE)]
      new_list_of_files <- paste0(raw_data_folder,"/",new_list_of_files)
      #new_list_of_files <- new_list_of_files[grepl(studies$code[j],new_list_of_files,ignore.case = TRUE)]
      # Schueep_NA this study requires additional data
      additional_file <- paste0(original_raw_data_folder,"/","rspb-2013-2667-file007.xls")
      list_of_files <- c(list_of_files,additional_file)
      
      new_additional_file <- paste0(raw_data_folder,"/","rspb-2013-2667-file007.xls")
      new_list_of_files <- c(new_list_of_files,new_additional_file)
    }
    else{
      list_of_files <- list_of_files[grepl(studies$old_code[j],list_of_files,ignore.case = TRUE)]
      new_list_of_files <- list.files(original_raw_data_folder)[grepl(studies$old_code[j],list.files(original_raw_data_folder),ignore.case = TRUE)]
      new_list_of_files <- paste0(raw_data_folder,"/",new_list_of_files)
      #new_list_of_files <- new_list_of_files[grepl(studies$code[j],new_list_of_files,ignore.case = TRUE)]
    }
    
    #Copy files
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    
  }else if(studies$Location[j]=="DAINESE 2019 DATABASE"){
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],studies$Raw_data_Folder[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    #new_list_of_files <- paste0(raw_data_folder,"/",list.files(original_raw_data_folder))
    
    # Select files for a given code
    
    if(studies$code[j] %in% c("Virginie_Boreux_Coffea_canephora_India_2008",
                              "Virginie_Boreux_Malus_domestica_Germany_2015")){
      new_code <- substr(studies$old_code[j], start = 1, stop = 6)
    }else if(studies$code[j] %in% c("Smitha_Krishnan_Coffea_canephora_India_2007",
                                    "Smitha_Krishnan_Coffea_canephora_India_2008",
                                    "Smitha_Krishnan_Coffea_canephora_India_2009",
                                    "Smitha_Krishnan_Coffea_canephora_India_2014")){
      new_code <- substr(studies$old_code[j], start = 1, stop = 7)
    }else{
      new_code <- substr(studies$old_code[j], start = 1, stop = 4)
    }
    
    list_of_files <- list_of_files[grepl(new_code,list_of_files,ignore.case = TRUE)]
    new_list_of_files <- list.files(original_raw_data_folder)[grepl(new_code,list.files(original_raw_data_folder),ignore.case = TRUE)]
    new_list_of_files <- paste0(raw_data_folder,"/",new_list_of_files)
    
    #new_list_of_files <- new_list_of_files[grepl(new_code,new_list_of_files,ignore.case = TRUE)]
    
    # "carv03" and "cavi01" studies require additional (taxon) data
    if(studies$code[j]=="LuÃ­sa_G_Carvalheiro_Mangifera_indica_South_Africa_2009"){
      
      additional_file <- paste0(R_file_location,"/","taxon_table_carv03.csv")
      list_of_files <- c(list_of_files,additional_file)
      
      new_additional_file <- paste0(base_study_i,"/","taxon_table_carv03.csv")
      new_list_of_files <- c(new_list_of_files,new_additional_file)
    }
    else if(studies$code[j]=="Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016"){
      
      additional_file <- paste0(R_file_location,"/","taxon_table_cavi01.csv")
      list_of_files <- c(list_of_files,additional_file)
      
      new_additional_file <- paste0(base_study_i,"/","taxon_table_cavi01.csv")
      new_list_of_files <- c(new_list_of_files,new_additional_file)
    }
    
    
    
    #Copy files
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    
  }else{
    
    original_raw_data_folder <- paste(folder_raw_data,studies$Location[j],sep="/") 
    list_of_files <- paste0(original_raw_data_folder,"/",list.files(original_raw_data_folder))
    list_of_files <- list_of_files[list_of_files!=R_file]
    
    new_R_file_raw <- paste0(base_study_i,"/Raw Data/",studies$code[j],".R")
    
    new_list_of_files <- paste0(raw_data_folder,"/",list.files(original_raw_data_folder))
    new_list_of_files <- new_list_of_files[new_list_of_files!=new_R_file_raw]
    
    #Copy files
    file.copy(from=list_of_files, to=new_list_of_files, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
  
}
  
}

# Update the study names in Gdocs
# gdocs_old_names <- read_csv("gdoc_newnames.csv") %>% rename(old_code=study)
# 
# gdocs_new_names <- data.folders %>% select(old_code,code)
# 
# gdocs_old_names <- gdocs_old_names %>% left_join(gdocs_new_names,by="old_code")
# 
# write_csv(gdocs_old_names,"gdocs_old_new_names.csv")
