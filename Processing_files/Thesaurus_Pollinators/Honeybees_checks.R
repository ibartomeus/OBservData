library(tidyverse)

# Load last thesaurus
thesaurus_updated <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META_V0p3.csv")


# Sanity Checks

list_honeybees <- thesaurus_updated %>% filter(Guild == "honeybees") %>%
  select(Organism_ID) %>% unique() #All honeybees are Apis mell, dorsata, cerana
# and florea

#Check guilds and names of organisms with cerana, dorsata o florea in their names

thesaurus_updated$Guild[grep("cerana",thesaurus_updated$Organism_ID,
                             ignore.case = T)]
thesaurus_updated$Organism_ID[grep("cerana",thesaurus_updated$Organism_ID,
                                   ignore.case = T)]

thesaurus_updated$Guild[grep("dorsata",thesaurus_updated$Organism_ID,
                             ignore.case = T)]
thesaurus_updated$Organism_ID[grep("dorsata",thesaurus_updated$Organism_ID,
                                   ignore.case = T)]

# OK: Andrenas are not Apis

thesaurus_updated$Guild[grep("florea",thesaurus_updated$Organism_ID,
                             ignore.case = T)]
thesaurus_updated$Organism_ID[grep("florea",thesaurus_updated$Organism_ID,
                                   ignore.case = T)]

# OK: Andrenas are not Apis

# Check that these organisms are not honeybees
thesaurus_updated$Guild[grepl("Melitomella",thesaurus_updated$Organism_ID,ignore.case = TRUE)]
thesaurus_updated$Guild[grepl("Mellisodes sp.",thesaurus_updated$Organism_ID,ignore.case = TRUE)]
thesaurus_updated$Guild[grepl("Nomada_marshamella",thesaurus_updated$Organism_ID,ignore.case = TRUE)]
thesaurus_updated$Guild[grepl("Nomada marshamella",thesaurus_updated$Organism_ID,ignore.case = TRUE)]
thesaurus_updated$Guild[grepl("Nomiapis bispinosa",thesaurus_updated$Organism_ID,ignore.case = TRUE)]
thesaurus_updated$Guild[grepl("Peponapis pruniosa",thesaurus_updated$Organism_ID,ignore.case = TRUE)]

#Find datasets with the organisms listed above

list_apis_dorsata <- c("Apis_dorsata","Apis_dorsata")

list_apis_cerana <- c("Apis cerana","Apis cerana","Apis cerana indica",
                      "Apis_cerana","Apis_cerana")
list_apis_florea <- c("Apis florea","Apis_florea")

list_other_wild <- c("Melitomella grisescens","Melitomella murihirta",
                     "Mellisodes sp.","Nomada_marshamella","Nomada marshamella",
                     "Nomiapis bispinosa","Peponapis pruniosa")

final_insect_sampling <-
  "Final_Data/CropPol_sampling_data.csv"

final_insect_sampling_data <- read_csv(final_insect_sampling)


final_insect_sampling_data %>%
  filter(pollinator %in% list_apis_dorsata) %>% dplyr::select(study_id,guild) %>%
  unique()

final_insect_sampling_data %>%
  filter(pollinator %in% list_apis_florea) %>% dplyr::select(study_id,guild) %>%
  unique()

final_insect_sampling_data %>%
  filter(pollinator %in% list_apis_cerana) %>% dplyr::select(study_id,guild) %>%
  unique()

final_insect_sampling_data %>%
  filter(pollinator %in% list_other_wild) %>% dplyr::select(study_id,guild) %>%
  unique()
