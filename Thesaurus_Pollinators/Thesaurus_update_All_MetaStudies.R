
library(tidyverse)

# Load thesaurus V0.2
thesaurus_old <- read_csv("Table_organism_guild_META_V0p2.csv")

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
      mutate(Data_version = "All_Meta")
    
    thesaurus_updated <- bind_rows(thesaurus_updated,new_entries)
    
  }
  
}

thesaurus_updated <- unique(thesaurus_updated)

#Add families to new data----


thesaurus_updated$Family[grepl("Agapostemon",thesaurus_updated$Organism_ID,ignore.case = TRUE)&
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Amegilla",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Andrena",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Anthophora",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Augochlora",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Augochlorella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Augochloropsis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Bombus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Ceratina",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Chalicodoma",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Coelioxys",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Colletes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Colletidae"
thesaurus_updated$Family[grepl("Creightonella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Dialictus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"

thesaurus_updated$Family[grepl("Epistrophe",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Eristalis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Eucera",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Eupeodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Halictidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Halictus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Heriades",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Hoplitis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Hylaeus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Colletidae"
thesaurus_updated$Family[grepl("Lasioglossum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Lasioglossum malachurum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version!="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Lipotriches",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Megachile",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Megasyrphus erraticus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Melipona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Melissodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Melitta",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Melittidae"
thesaurus_updated$Family[grepl("Nannotrigona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Nomada",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Osmia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Panurginus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Parasyrphus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Partamona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Platycheirus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Plebia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Psithyrus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Sphaerophoria",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Sphecodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Stratiomydae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Stratiomyidae"
thesaurus_updated$Family[grepl("Thyreus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Trigona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Xylocopa",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"

thesaurus_updated$Family[grepl("Acraea_horta",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Nymphalidae"
thesaurus_updated$Family[grepl("Anasimyia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Andrenidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Apidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Apis_millifera_scutellata",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Astylus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Melyridae"
thesaurus_updated$Family[grepl("Atylotus rusticus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tabanidae"
thesaurus_updated$Family[grepl("B..",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Bombyliidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Bombyliidae"
thesaurus_updated$Family[grepl("brown_skipper",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Hesperiidae"
thesaurus_updated$Family[grepl("Caliphoridae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Caliophoridae"
thesaurus_updated$Family[grepl("Cetoniidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Cetoniidae"
thesaurus_updated$Family[grepl("Chamaesphecia_anthraciformis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Sesiidae"

thesaurus_updated$Family[grepl("Chloromyia fromosa",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Stratiomyidae"
thesaurus_updated$Family[grepl("Chrysoperla",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Chrysopidae"
thesaurus_updated$Family[grepl("Chrysotoxum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Chrysotus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Dolichopodidae"
thesaurus_updated$Family[grepl("Cleridae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Cleridae"
thesaurus_updated$Family[grepl("Coeliades_pisistratus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Hesperiidae"
thesaurus_updated$Family[grepl("Colias",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Pieridae"
thesaurus_updated$Family[grepl("Colletidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Colletidae"
thesaurus_updated$Family[grepl("Colotis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Pieridae"
thesaurus_updated$Family[grepl("Danus_chrysippus_",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Nymphalidae"
thesaurus_updated$Family[grepl("Dasypoda",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Melittidae"
thesaurus_updated$Family[grepl("Dasysyrphus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Empididae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Empididae"
thesaurus_updated$Family[grepl("Eucerini",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Evylaeus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Exomalopsis",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Fulgoridae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Fulgoridae"
thesaurus_updated$Family[grepl("Gymnosoma",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tachinidae"

thesaurus_updated$Family[grepl("Haematopora",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tabanidae"
thesaurus_updated$Family[grepl("Haemmatopota",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tabanidae"
thesaurus_updated$Family[grepl("Halictini",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Helophillus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Hesperiidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Hesperiidae"
thesaurus_updated$Family[grepl("Hypolimnas_misippus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Nymphalidae"
thesaurus_updated$Family[grepl("Lace_bug",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tingidae"
thesaurus_updated$Family[grepl("ladybird",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Coccinellidae"
thesaurus_updated$Family[grepl("Leucozona.lucorum",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Lycaenidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Lycaenidae"
thesaurus_updated$Family[grepl("Lygus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Miridae"
thesaurus_updated$Family[grepl("Macroglossum_trochilus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Sphingidae"
thesaurus_updated$Family[grepl("Megachilidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Megachilidae"
thesaurus_updated$Family[grepl("Melangyna cincta",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Melanogaster",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Melanostoma",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Melipona",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Meliscaeva.cinctella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Muscid",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Muscidae"
thesaurus_updated$Family[grepl("Myiatropa",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Mylothris",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Pieridae"
thesaurus_updated$Family[grepl("Neocorynura",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Noctuidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Noctuidae"
thesaurus_updated$Family[grepl("Orthonevra",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Plebeia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Precis_oenone",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Nymphalidae"
thesaurus_updated$Family[grepl("Psaenythia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Andrenidae"
thesaurus_updated$Family[grepl("Psythirus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Rhagio tringarius",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Rhagionidae"
thesaurus_updated$Family[grepl("Rhangio tringarius",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Rhagionidae"
thesaurus_updated$Family[grepl("Sarcophagidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Sarcophagidae"
thesaurus_updated$Family[grepl("Scathophaga",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Scathophagidae"
thesaurus_updated$Family[grepl("Schwarziana.quadripunctata",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("small_hover_",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Specodes",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Halictidae"
thesaurus_updated$Family[grepl("Sphecidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Sphecidae"
thesaurus_updated$Family[grepl("Syrphus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Systoechus ctenopterus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Bombyliidae"
thesaurus_updated$Family[grepl("Tabanus bovine",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tabanidae"
thesaurus_updated$Family[grepl("Talmerus atricapillus",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Asilidae"
thesaurus_updated$Family[grepl("Tenebrionidae",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Tenebrionidae"
thesaurus_updated$Family[grepl("Tetragonisca.fiebrigi",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Tetraloniella",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Thygater",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Apidae"
thesaurus_updated$Family[grepl("Tropidia",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"
thesaurus_updated$Family[grepl("Volucella pelluceris",thesaurus_updated$Organism_ID,ignore.case = TRUE) &
                           thesaurus_updated$Data_version=="All_Meta"] <- "Syrphidae"

sum(is.na(thesaurus_updated$Family))

#recap----
thesaurus_updated %>% group_by(Guild)%>% count() #23 missing; 

# save list
write_csv(thesaurus_updated,"Table_organism_guild_META_V0p3.csv")
