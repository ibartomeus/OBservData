
library(tidyverse)

dir_ini <- getwd()
folder_base <- "C:/Users/USUARIO/Desktop/OBservData/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "insect_sampling"

list_insect_sampling_files <- files_base[grepl("insect_sampling", files_base)]



  
excluded_methods <- c("Pan-traps","Pantrap","pan trap","hand sampling","pitfall",
                    "Pan traps","pan_trap")


# We look for organism with abundance > 5%

for (i in 1:length(list_insect_sampling_files)){

  print(list_insect_sampling_files[i])
  
  insect_sampling_i <- paste(folder_base, list_insect_sampling_files[i], sep = "/")
  data.site <- read_csv(insect_sampling_i)
  
  data.site.filt <- data.site %>% filter(!sampling_method %in% excluded_methods,
                                         !is.na(sampling_method),!is.na(pollinator))
  
  data.site.poll <- data.site.filt %>% group_by(study_id,pollinator,guild,sampling_method) %>% count()
  #data.site.guild <- data.site.filt %>% group_by(study_id,guild,sampling_method) %>% count()
  data.site.guild <- data.site.filt %>% group_by(study_id,sampling_method) %>% count()
  
  # data.site.poll <- data.site.poll %>%
  #   left_join(data.site.guild, by=c("study_id","guild","sampling_method")) %>%
  #   mutate(percentage = 100 * n.x / n.y)
  
  data.site.poll <- data.site.poll %>%
    left_join(data.site.guild, by=c("study_id","sampling_method")) %>%
    mutate(percentage = 100 * n.x / n.y)
  
  data.site.poll <- data.site.poll %>% filter(percentage >= 5) %>%
    select(pollinator,guild,percentage,sampling_method)
  
  if (i==1){
    list_main_pollinators  <- data.site.poll 
  } else {
    list_main_pollinators <- list_main_pollinators %>% bind_rows(data.site.poll) 
  }
}

# Add coutry, crop, variety from field_level files

list_field_level_files <- files_base[grepl("field_level", files_base)]

for (i in 1:length(list_field_level_files)){
  
  print(list_field_level_files[i])
  
  field_level_i <- paste(folder_base, list_field_level_files[i], sep = "/")
  data.site <- read_csv(field_level_i) %>% select(study_id,crop,variety,country)
  study_i <- unique(data.site$study_id)
  
  if (i==1){
    list_main_pollinators_i <- list_main_pollinators %>% filter(study_id==study_i)
    list_main_pollinators_complete <- list_main_pollinators_i %>% left_join(data.site,by=c("study_id"))
  }else{
    
    list_main_pollinators_i <- list_main_pollinators %>% filter(study_id==study_i)
    list_main_pollinators_i <- list_main_pollinators_i %>% left_join(data.site,by=c("study_id"))
    list_main_pollinators_complete <- bind_rows(list_main_pollinators_complete,list_main_pollinators_i)
  }
}

#####################################
# Test sampling methods' sanity
#####################################

unique(list_main_pollinators_complete$sampling_method[grepl("pan",
                                                            list_main_pollinators_complete$sampling_method,
                                                            ignore.case = TRUE)])
#PAN generate No matches

unique(list_main_pollinators_complete$sampling_method[grepl("pit",
                                                            list_main_pollinators_complete$sampling_method,
                                                            ignore.case = TRUE)])
#PIT generate No matches
unique(list_main_pollinators_complete$sampling_method[grepl("hand",
                                                            list_main_pollinators_complete$sampling_method,
                                                            ignore.case = TRUE)])
#HAND generate No matches

#######################
# FILTER ODD ORGANISMS
#######################

list_main_pollinators_complete <- list_main_pollinators_complete  %>% 
  filter(!grepl("Avispa",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("solitary bee",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("honeybee",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("identified",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("pest_",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("native_bee",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("negro",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("brillante",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("mariposa",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!(grepl("fly",pollinator,ignore.case = T) & !grepl("mark",pollinator,ignore.case = TRUE)))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("Lucillia",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("gen_sp",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("other",pollinator,ignore.case = TRUE))

list_main_pollinators_complete <- list_main_pollinators_complete %>% 
  filter(!grepl("spider",pollinator,ignore.case = TRUE))



list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "A. mellifera"] <- "Apis mellifera"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Apis_mellifera"] <- "Apis mellifera"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. hortorum"] <- "Bombus hortorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. hypnorum"] <- "Bombus hypnorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. lapidarius"] <- "Bombus lapidarius"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. pascuorum"] <- "Bombus pascuorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. pratorum"] <- "Bombus pratorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "B. terrestris"] <- "Bombus terrestris"



list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Bombus_hortorum"] <- "Bombus hortorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Bombus_lucorum/terrestris"] <- "Bombus terrestris or lucorum"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Bombus_lapidarius"] <- "Bombus lapidarius"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Bombus_pascuorum"] <- "Bombus pascuorum"



list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Nomada"] <- "Nomada sp"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Osmia"] <- "Osmia sp"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "honey_bee"] <- "Apis mellifera"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Ladybirds"] <- "Coccinellidae"

list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Apis_cerana"] <- "Apis cerana"
list_main_pollinators_complete$pollinator[list_main_pollinators_complete$pollinator == "Apis_dorsata"] <- "Apis dorsata"


list_main_pollinators_complete$pollinator <- str_replace(list_main_pollinators_complete$pollinator, "_", " ")

#######################
# Save results
#######################


brief_list <- list_main_pollinators_complete %>% group_by(country,crop,pollinator,guild) %>%
  count() %>% select(-n)

setwd(dir_ini)
write_csv(brief_list, "Summary_dominant_species.csv")

list_main_pollinators_complete <- 
  select(list_main_pollinators_complete,study_id,country,crop,variety,
          pollinator,guild,percentage,sampling_method)

write_csv(list_main_pollinators_complete, "Full_report_dominant_species.csv")


