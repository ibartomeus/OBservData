
library(tidyverse)

# Load thesaurus V0.1
thesaurus_old <- read_csv("Table_organism_guild_META.csv")
thesaurus_old$Data_version <- "V0.1"

thesaurus_updated <- thesaurus_old 

folder_base <- "../Datasets_storage"
files_base <- list.files(folder_base)
list_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]

for (i in 1:length(list_files_insect_sampling)){
  
  file_insect_sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  insect_sampling_i <- read_csv(file_insect_sampling_i)
  
  list_Organism_ID <- unique(insect_sampling_i$pollinator)
  
  new_ID <- list_Organism_ID[!list_Organism_ID %in% thesaurus_old$Organism_ID]
  
  if (length(new_ID)>0){
    
    new_entries <- insect_sampling_i %>% filter(pollinator %in% new_ID) %>% 
      select(pollinator,guild) %>% unique() %>% rename(Organism_ID=pollinator,Guild=guild) %>%
      mutate(Data_version = "Kleijn")
    
    thesaurus_updated <- bind_rows(thesaurus_updated,new_entries)
    
  }
  
}

thesaurus_updated <- unique(thesaurus_updated)

#Add families to new data----

new_Organisms <- thesaurus_updated %>% filter(Data_version == "Kleijn") %>% 
  pull(Organism_ID)

library(taxize)
names_check <- gnr_resolve(names = new_Organisms)

#Filter results by using NCBI database and the following criteria:
# The number of words in the submitted name should be larger than or equal to 
# that of matched name 

names_NCBI <- names_check %>% filter(data_source_title=="NCBI",
                                     sapply(strsplit(names_check$submitted_name, " "), length)>=
                                       sapply(strsplit(names_check$matched_name, " "), length))


taxons <- tibble(submitted_name=names_NCBI$submitted_name,matched_name=names_NCBI$matched_name)
taxons$Family <- NA

ranks <- classification(names_NCBI$matched_name, db = "ncbi")

for (i in 1:nrow(taxons)){
  print(i)
  
  if(!is.na(ranks[taxons$matched_name[i]])){
    rank_i <- ranks[taxons$matched_name[i]]
    taxons$Family[i] <- rank_i[[1]]$name[rank_i[[1]]$rank=="family"]
  }
}

#Fix Families----


taxons$Family[grepl("Andrena sp. aff. minutula",taxons$Family,ignore.case = TRUE)] <- "Andrenidae"
taxons$Family[grepl("Lasioglossum evylaeus sp.",taxons$Family,ignore.case = TRUE)] <- "Halictidae"
taxons$Family[grepl("Amegilla (sulawesi) sp. aff. samarensis",taxons$Family,ignore.case = TRUE)] <- "Apidae"
taxons$Family[grepl("Patellapis (pachyhalictus, sulawesi) sp.",taxons$Family,ignore.case = TRUE)] <- "Halictidae"
taxons$Family[grepl("Megachile (sulawesi) sp. aff. bakeri",taxons$Family,ignore.case = TRUE)] <- "Megachilidae"
taxons$Family[grepl("Amegilla (sulawesi) sp. zonata-group",taxons$Family,ignore.case = TRUE)] <- "Apidae"

taxons_fixed <- taxons %>% select(submitted_name,Family) %>% 
  rename(Organism_ID=submitted_name) %>% unique() 


for(i in 1:nrow(taxons_fixed)){
  
  thesaurus_updated$Family[thesaurus_updated$Organism_ID==taxons_fixed$Organism_ID[i] &
                             thesaurus_updated$Data_version=="Kleijn"] <- taxons_fixed$Family[i]
}


thesaurus_updated$Family[grepl("Agapostemon",thesaurus_updated$Organism_ID,ignore.case = TRUE)&
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Amegilla",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Andrena",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Anthophora",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Augochlora",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Augochlorella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Augochloropsis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Bombus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Ceratina",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Chalicodoma",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Coelioxys",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Colletes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Colletidae"
thesaurus_updated$Family[grepl("Creightonella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Dialictus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"

thesaurus_updated$Family[grepl("Epistrophe",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Eristalis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Eucera",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Eupeodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Halictidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Halictus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Heriades",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Hoplitis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Hylaeus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Colletidae"
thesaurus_updated$Family[grepl("Lasioglossum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Lasioglossum malachurum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version!="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Lipotriches",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Megachile",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Megasyrphus erraticus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Melipona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Melissodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Melitta",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Melittidae"
thesaurus_updated$Family[grepl("Nannotrigona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Nomada",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Osmia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Panurginus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Parasyrphus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Partamona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Platycheirus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Plebia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Psithyrus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Sphaerophoria",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Sphecodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Halictidae"
thesaurus_updated$Family[grepl("Stratiomydae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Stratiomyidae"
thesaurus_updated$Family[grepl("Thyreus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Trigona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"
thesaurus_updated$Family[grepl("Xylocopa",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="Kleijn"] <- "Apidae"


#recap----
thesaurus_updated %>% group_by(Guild)%>% count() #20 missing; 

# save list
write_csv(thesaurus_updated,"Table_organism_guild_META_V0p2.csv")
