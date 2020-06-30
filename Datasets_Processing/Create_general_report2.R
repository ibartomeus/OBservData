

create_general_report <- function(authors,i,studies,base_author_i,folder_procc_data){
  
  file_name <- paste0("First read me - General report ",authors[i],".txt")
  file_name <- paste(base_author_i,file_name,sep = "/")
  
  file_name_RMD <- paste0("First read me - General report ",authors[i],".Rmd")
  file_name_RMD <- paste(base_author_i,file_name_RMD,sep = "/")
  
  file_name_docx <- paste0("First read me - General report ",authors[i],".docx")
  file_name_docx <- paste(base_author_i,file_name_docx,sep = "/")
  
  dir_ini <- getwd()
  
  ###############################
  # REPORT LINES
  ###############################
  
  report_lines <- c(paste0("Dear ",authors[i],","),
                    "",
                    "According to our records, you are the corresponding author of the following studies:",
                    "")
  
  # List studies for author_i
  
  studies <- data.folders %>% filter(author_folder==authors[i])
  
  for (j in 1:nrow(studies)){
    
    study_i <- studies$code[j]
    
    
    if(studies$Location[j]=="KLEIJN 2015 DATABASE"){
      Meta = "Raw data were collected from Kleijn et al.,Nat. Comm. 2015; 6:7414."
    }else if(studies$Location[j]=="GARIBALDI 2015 DATABASE"){
      Meta = "Raw data were collected from  Garibaldi et al., J. Appl. Ecol. 2015; 52, 6."
    }else if(studies$Location[j]=="GARIBALDI 2016 DATABASE"){
      Meta = "Raw data were collected from Garibaldi et al., Science 2016; 351: 6271."
    }else if(studies$Location[j]=="DAINESE 2019 DATABASE"){
      Meta = "Raw data were collected from Dainese et al., Sci. Adv. 2019; 5: eaax0121."
    }else if(studies$Location[j]=="RADER 2016 DATABASE"){
      Meta = "Raw data were collected from Rader et al., PNAS 2016; 113, 1."
    }else{
      Meta = ""
    }
    
    dat.site_i <- read_csv(paste(folder_procc_data,studies$file_field[j], sep = "/"))
    
    num_sites_i <- nrow(dat.site_i) 
    
    if (length(unique(dat.site_i$crop))>1){
      crop_i <- "several crops"
    }else{
      crop_i <- unique(dat.site_i$crop)
    }
    
    if (length(unique(dat.site_i$country))>1){
      country_i <- "several countries"
    }else{
      country_i <- unique(dat.site_i$country)
    }
    
    if (length(unique(dat.site_i$sampling_year))>1){
      year_i <- "several years"
    }else{
      year_i <- unique(dat.site_i$sampling_year)
    }
    
    report_lines <- c(report_lines,
                      paste0("- ",study_i," (Study ID): ",
                             #crop_i,", ",
                             #country_i,", ",
                             #year_i,", ",
                             num_sites_i," site/s. ",
                             Meta)
                      )
  }
  
  report_lines <- c(report_lines,"",
                    "Please confirm that (i) you are the correct corresponding author of the listed studies, and (ii) we have your permission to re-use your data, and (iii) list all co-author/co-owners affiliations and acknowledgements/funding information in your 'Data_ownership' (excel) file.",
                    "",
                    "The above datasets have been processed in line with OBServ guidelines (see 'General information about OBServ data processing' at the end of this file). In order to check the correctness of the results obtained from the processing stage, we would like to share them with you, along with a report and some queries. To do so, we have created one folder for each study. There you can find the following items:",
                    "",
                    "- One folder that contains the raw data that was sent to us.",
                    "- One 'field_level_data' (csv) file with the processed data.",
                    "- One 'insect sampling' (csv) file, if sampling information was provided to us.",
                    "- One 'Explanation column names' (xlsx) file with auxiliary information to interpret the data in 'field_level_data' and 'insect_sampling' files.",
                    "- One 'Summary report and queries' (pdf) file generated from your corresponding 'field_level_data' file.",
                    "- One folder with background information on data file conversion. It contains at least one R script, whose name is the study ID. This file was used to process the raw data and generate the previous csv files. Thus, if needed, you can go through all the decisions that were taken to process your file. In case you want to run the script notice that paths and working directories should be updated to your relative local paths. Besides, if your R script needs additional input files or post-processing (such as 'Table_organism_guild_META.csv', 'taxon_table.csv', or 'add_taxon_constraint_column_DAINESE_RADER_OTHER.R'), such files have been also included in the 'background information on data file conversion' folder.",
                    "",
                    "Please review at least the 'field_level_data' and the 'Summary_report_and_queries' files, answer the queries in the latter and send your answers/comments back to us, along with your 'Data_ownership' (excel) file.",
                    "",
                    "",
                    "If you have any questions, please do not hesitate to contact us. ",
                    "",
                    "Best regards,",
                    "",
                    "Alfonso Allen-Perkins and",
                    "the OBServ Team",
                    "",
                    "================================================",
                    "General information about OBServ data processing",
                    "================================================",
                    "",
                    "If several censuses were conducted in a given field/orchard, we recorded the total sampling effort that was made, that is, the total sampling time [in minutes] and the total sampled area [in m^2]. ",
                    "",
                    "- The values of richness, abundance and visitation rates for a given site are obtained by aggregating the records of insects that were observed during the total sampling time. Consequently, in this database richness, abundance and visitation rates do not reflect the mean value of the respective surveys or rounds in each site, but the total one. ",
                    "",
                    "- Pan-traps data were not taken into account to estimate the values of richness and abundance, respectively, in each site. ",
                    "",
                    "- When possible, visitation rates were only derived from timed observations to a given number of flowers, and their units were set to [visits per 100 flowers and hour].",
                    "",
                    "- Richness data were not recorded if the percentage of identified species (or morphospecies) was lower than or (approximately) equal to 75%.",
                    "")
  

  # fileConn <- file(file_name)
  # writeLines(report_lines, fileConn)
  # close(fileConn)
  #Encoding(report_lines) <- "UTF-8"
  
  cat(report_lines, sep="  \n", file = file_name_RMD)
  
  setwd(base_author_i)
  
  render(file_name_RMD, pdf_document(),encoding="WINDOWS-1252")
  setwd(dir_ini)
  file.remove(file_name_RMD) #cleanup
  
}

