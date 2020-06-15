#https://ropensci.org/tutorials/taxize_tutorial/

library(tidyverse)
library(openxlsx)
library(taxize)
library(stringr)

data.species <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

names(data.species)[12] <- "NOTES_1"
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance_corrected= `Abundance.(corrected.for.differences.in.sampling.effort)`,
                                        Organism_ID=OrganismID,abundance_NO_corrected= `Abundance.(no.correction.for.sampling.effort)`)

data.species_01 <- data.species %>% filter(grepl("carv03",data.species$site_id,ignore.case = TRUE))


pollinator <- unique(data.species_01$Organism_ID)

##################################################################
# Find morphospecies (entries with the combination sp + number)
##################################################################

# Visualize candidates to morphospecies
pollinator[grepl("sp",pollinator,ignore.case = TRUE)]

# Identify entries with sp
morpho_entries <- str_match(pollinator, "^.+sp(.*)")
morpho_entries[morpho_entries[,2]=="_",2] <- NA
morpho_entries[morpho_entries[,2]=="",2] <- NA

# Entries with sp+number -> Morphos
entries_with_number <- !is.na(morpho_entries[,2])



morpho_taxon <- tibble(submitted_name=pollinator[entries_with_number],matched_name=submitted_name,
                       rank="morphospecies")

# Inspect the remaining organisms
candidates <- pollinator[!entries_with_number]

names_check <- gnr_resolve(names = candidates)

#Filter results by using NCBI database and the following criteria:
# The number of words in the submitted name should be larger than or equal to 
# that of matched name 

names_NCBI <- names_check %>% filter(data_source_title=="NCBI",
                                     sapply(strsplit(names_check$submitted_name, " "), length)>=
                                       sapply(strsplit(names_check$matched_name, " "), length))

taxons <- tibble(submitted_name=names_NCBI$submitted_name,matched_name=names_NCBI$matched_name)
taxons$rank <- NA

ranks <- classification(names_NCBI$matched_name, db = "ncbi")

for (i in 1:nrow(taxons)){
  print(i)
  rank_i <- ranks[taxons$matched_name[i]]
  taxons$rank[i] <- rank_i[[1]]$rank[rank_i[[1]]$name==taxons$matched_name[i]]
}

other_taxons <- tibble(submitted_name=candidates[!candidates %in% names_NCBI$submitted_name])
other_taxons$matched_name <- NA
other_taxons$rank <- NA

for (i in 1:nrow(other_taxons)){
  print(other_taxons$submitted_name[i])
  rank_i <- classification(other_taxons$submitted_name[i], db = "ncbi")
  other_taxons$matched_name[i] <- rank_i[[1]][nrow(rank_i[[1]]),1]
  other_taxons$rank[i] <- rank_i[[1]][nrow(rank_i[[1]]),2]
  
}

for (i in 1:nrow(other_taxons)){
  if (is.na(other_taxons$rank[i])){
    print(other_taxons$submitted_name[i])
    rank_i <- classification(other_taxons$submitted_name[i], db = "eol")
    other_taxons$matched_name[i] <- rank_i[[1]][nrow(rank_i[[1]]),1]
    other_taxons$rank[i] <- rank_i[[1]][nrow(rank_i[[1]]),2]
  }
}

taxon_tibble <- bind_rows(morpho_taxon,taxons,other_taxons) %>%
  rename(pollinator=submitted_name)

# Fix errors
other_species <- c("Apis_mellifera","Chrysomya_marginalis__Wiedemann_",
                   "Hippodamia.variegata..goeze.","Heteroptera_2mm","Heteroptera_5mm",
                   "Hippodamia.variegata..Goeze.",
"Exochomus_troberti_Mulsant_1850","Harmonia_axyridis_I_158","Monolepta_ursulae_Wagner_2003",
"Pedinorrhina__Chondrorrhina__trivittata__Schaum")
taxon_tibble$rank[taxon_tibble$pollinator%in%other_species] <- "species"


study_id <- "carv03"

write_csv(taxon_tibble,paste0("taxon_table_",study_id,".csv"))
