#https://ropensci.org/tutorials/taxize_tutorial/

library(tidyverse)
library(openxlsx)
library(taxize)
library(stringr)

dir_ini <- getwd()

##########################
#Data: Rader et al. 2016 
##########################

data_raw <- read.xlsx("master_nonbeemay24sep14.xlsx",
                      sheet = "master_nonbeemay2814", startRow = 1)

##########################################
# FIXING COLUMNS' NAMES
# Two columns names are repeated and they contain different values, respectively
#"Sphaerophoria_sp." "Syritta_pipiens"
# Solution proposed: To merged columns with the same name

# Fix format of observations
data_raw[,30:ncol(data_raw)] <- sapply(data_raw[,30:ncol(data_raw)], as.numeric)


names_data_raw <- names(data_raw)
names_data_raw[duplicated(names_data_raw)]
#"Sphaerophoria_sp." "Syritta_pipiens"
names(data_raw)[408]==names(data_raw)[412]


which(colnames(data_raw)=="Sphaerophoria_sp.")
sum(!is.na(data_raw[,408]))
sum(!is.na(data_raw[,412]))

# Combine both columns and delete one

data_raw[,408] <- rowSums(data_raw[,c(408, 412)], na.rm=TRUE)
data_raw[,412] <- NULL #remove column

# Sanity check: new column 412 is different from column 408
names(data_raw)[408]==names(data_raw)[412]

which(colnames(data_raw)=="Syritta_pipiens")
sum(!is.na(data_raw[,418]))==sum(!is.na(data_raw[,432]))

data_raw[,418] <- rowSums(data_raw[,c(418, 432)], na.rm=TRUE)
data_raw[,432] <- NULL
# Sanity check: new column 418 is different from column 432
names(data_raw)[418]==names(data_raw)[432]

########################################
# EXTRACT ORGANISM NAMES
########################################

data_raw <- as_tibble(data_raw)

#authors <- data_raw %>% group_by(author,crop,Year_of_study) %>% count()
#write.csv(authors,"authors_list.csv")

#Remove studies that were included in Dainese et al. 2019

authors_in_Dainese <- c("Anderson","Bartomeus","Carvalheiro",
                        "Chacoff","Freitas","Garratt_potts",
                        "garrett","Howlett","Stanley_stout",
                        "taki","smitha")

data_filter <- data_raw %>% filter(!author %in% authors_in_Dainese)

# Remove columns full of NA's
data_filter_without_NAs <- 
  data_filter[,colSums(is.na(data_filter))<nrow(data_filter)]


pollinator <- unique(names(data_filter_without_NAs[,28:ncol(data_filter_without_NAs)]))

pollinator <- str_replace(pollinator,"_"," ")

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

names_NCBI <- names_check %>% filter(data_source_title =="NCBI",
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

#Following queries to NCBI do not work
for (i in 1:nrow(other_taxons)){
  print(other_taxons$submitted_name[i])
  rank_i <- classification(other_taxons$submitted_name[i], db = "ncbi")
  other_taxons$matched_name[i] <- rank_i[[1]][nrow(rank_i[[1]]),1]
  other_taxons$rank[i] <- rank_i[[1]][nrow(rank_i[[1]]),2]
  
}

#Following queries to EOL do not work
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


order_list <- c("hymenoptera sp.")

family_list <- c("Otitidae","syrphidae","Bombylidae","formicidae")

genus_list <- c("Salpinigaster sp")
  
species_list <- c("Bombus abuterraneus",
                  "Bombus lucorum_terrestris","Bombus terrestris_aggregate",
                  "Sphaerophoria cf_scripta","Macrodactylus fulvescens",
                  "Osmia rufa","Sphaerophoria interrupta","Bibo hortulans","Bibo marci",
                  "Bibo varipes","Dilophilus febrilis","Eristalinus sepulchralis",
                  "Eristalis horticola","Eristalis lineata","Eristalis intricaria",
                  "Eristalis nemorum","Eristalis pratorum","Eristalis similis",
                  "Macrodactylus fulvescens","Melanogaster hirtella","Myathropa florea",
                  "Osmia rufa","Parhelophilus frutetorum","Rhingia rostrata",
                  "Sphaerophoria interrupta","Tropidia scita","Halictus tumulorum",
                  "Helophilus trivittatus")

mophospecies_list <- c("Syrphid 1","Syrphid 2","Syrphid 3","Syrphid 1_vergara",
                       "Syrphid 2_vergara","Syrphid 3_vergara","Andrena 2_brittaindataset",
                       "Andrena 3_brittaindataset","Andrena 4_brittaindataset","Bombus 6",
                       "Bombus 7","Dialictus 12","Diptera 2","Diptera 3",
                       "Diptera 4","	Drosophila 5","Eucera 16","Evylaeus 13","Habropoda 17",
                       "Lasioglossum 8","Lasioglossum 9","Lasioglossum 10","Lasioglossum 11",
                       "Lasioglossum 14","Muscidae 6","Muscidae 7","Osmia 18","Osmia 19",
                       "Panurginus 15","Anthophora 5")
                       
                       
                       
taxon_tibble$rank[taxon_tibble$pollinator %in% family_list] <- "family"
taxon_tibble$rank[taxon_tibble$pollinator %in% genus_list] <- "genus"
taxon_tibble$rank[taxon_tibble$pollinator %in% order_list] <- "order"
taxon_tibble$rank[taxon_tibble$pollinator %in% species_list] <- "species"
taxon_tibble$rank[taxon_tibble$pollinator %in% mophospecies_list] <- "morphospecies"



study_id <- "Rader"

write_csv(taxon_tibble,paste0("taxon_table_",study_id,".csv"))
