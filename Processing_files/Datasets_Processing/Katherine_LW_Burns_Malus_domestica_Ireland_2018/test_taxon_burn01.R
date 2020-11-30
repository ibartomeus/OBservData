#https://ropensci.org/tutorials/taxize_tutorial/

library(tidyverse)
library(openxlsx)
library(taxize)
library(stringr)

data_sampling <- read.xlsx("Processing_files/Datasets_Processing/Katherine_LW_Burns_Malus_domestica_Ireland_2018/globalcrop.APPLE.BurnsStanley.xlsx",
                       sheet = "insect_sampling", startRow = 1)

data_sampling <- as_tibble(data_sampling)

pollinator <- unique(data_sampling$pollinator)

##################################################################
# Find morphospecies (entries with the combination sp + number)
##################################################################

# Visualize candidates to morphospecies
pollinator[grepl("sp",pollinator,ignore.case = TRUE)]

# Identify entries with sp
morpho_entries <- str_match(pollinator, "^.+sp(.*)")

# Entries with sp+number -> Morphos
entries_with_number <- grepl("[[:digit:]]", morpho_entries[,2])

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

study_id <- "barn01"

write_csv(taxon_tibble,paste0("Processing_files/Datasets_Processing/Katherine_LW_Burns_Malus_domestica_Ireland_2018/taxon_table_",study_id,".csv"))
