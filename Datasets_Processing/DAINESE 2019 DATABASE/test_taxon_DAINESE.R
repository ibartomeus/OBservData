
library(tidyverse)
library(openxlsx)

data_dainese <- NULL

######################

data.species <- read.xlsx("DATASETS/Saez01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID) %>% 
  mutate(study_id = "Agustin_Saez_Rubus_idaeus_Argentina_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)

#################################

data.species <- read.xlsx("DATASETS/Clas01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)%>% 
  mutate(study_id = "Alice_Claßen_Coffea_arabica_Tanzania_2011_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)
##################################

data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI01")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2011,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Anacardium_occidentale_Brazil_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)

###############################################
data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI01")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>%
  filter(sampling_year==2011,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Anacardium_occidentale_Brazil_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)

#######################################

data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI02")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)


data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2012,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Anacardium_occidentale_Brazil_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
####################################
data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI07")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2013,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Annona_muricata_Brazil_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
##############################################
data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI06")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2013,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Annona_squamosa_Brazil_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
#############################
data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI04")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2007,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Bixa_orellana_Brazil_2007") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
#############################


data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI05")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2011,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Gossypium_hirsutum_Brazil_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
###################################
data.site <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="FREI03")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2011,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "Breno_M_Freitas_Malpighia_emarginata_Brazil_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
###############################

data.species <- read.xlsx("DATASETS/Radewill01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2016)%>% 
  mutate(study_id = "Bryony_Willcox_Mangifera_indica_Australia_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
################################

data.species2 <- read.xlsx("DATASETS/Scil01_Datacollection_pollination.xlsx",
                           sheet = "SpeciesData_all", startRow = 2)

data.species2 <- as_tibble(data.species2)
data.species2 <- data.species2 %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                          sampling_method=Sampling.method,abundance=Abundance,
                                          Organism_ID=OrganismID)%>% 
  mutate(study_id = "Claire_Kremen_Fragaria_ananassa_USA_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species2)

data.species2 <- read.xlsx("DATASETS/Scil01_Datacollection_pollination.xlsx",
                           sheet = "SpeciesData_all", startRow = 2)

data.species2 <- as_tibble(data.species2)
data.species2 <- data.species2 %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                          sampling_method=Sampling.method,abundance=Abundance,
                                          Organism_ID=OrganismID)%>% 
  mutate(study_id = "Claire_Kremen_Fragaria_ananassa_USA_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species2)

data.species <- read.xlsx("DATASETS/Scil01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData_bees", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)%>% 
  mutate(study_id = "Claire_Kremen_Fragaria_ananassa_USA_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)

data_dainese <- unique(data_dainese)
#######################################################

data.species <- read.xlsx("DATASETS/Stan01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)%>% 
  mutate(study_id = "Dara_Stanley_Brassica_napus_Ireland_2010") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)

#######################################################

data.species <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 4)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance.not.corrected.for.sampling.effort,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>%
  mutate(study_id = "Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#######################################################
data.site <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Study=="Groo01")
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=Study,site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

#Somehow three columns [7,8,9] appear repeated-> FIXING PROBLEM:
data.species <- data.species[-c(7,8,9)]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  filter(sampling_year==2013,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "David_Kleijn_Malus_domestica_Netherlands_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#######################################

data.site <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Study=="Groo02")
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=Study,site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

#Somehow three columns [7,8,9] appear repeated-> FIXING PROBLEM:
data.species <- data.species[-c(7,8,9)]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_02 <- data.species %>% 
  filter(sampling_year==2014,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "David_Kleijn_Malus_domestica_Netherlands_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_02)
#################################################


data.site <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Study=="Groo03")
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=Study,site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

#Somehow three columns [7,8,9] appear repeated-> FIXING PROBLEM:
data.species <- data.species[-c(7,8,9)]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% 
  filter(sampling_year==2013,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "David_Kleijn_Vaccinium_corymbosum_Netherlands_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
###############################################

data.site <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                       sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Study=="Groo04")
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=Study,site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.species <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

#Somehow three columns [7,8,9] appear repeated-> FIXING PROBLEM:
data.species <- data.species[-c(7,8,9)]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_04 <- data.species %>% 
  filter(sampling_year==2014,site_id %in% data.site$site_id)%>% 
  mutate(study_id = "David_Kleijn_Vaccinium_corymbosum_Netherlands_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_04)
#############################################

data.species <- read.xlsx("DATASETS/Ande01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)



data.species_01 <- data.species %>% filter(sampling_year==2010)%>% 
  mutate(study_id = "Georg_Andersson_Brassica_rapa_Sweden_2010") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
############################################

data.species <- read.xlsx("DATASETS/Ande01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)


data.species_01 <- data.species %>% filter(sampling_year==2009) %>% 
mutate(study_id = "Georg_Andersson_Fragaria_ananassa_Sweden_2009") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(study_id=="conn01")%>% 
  mutate(study_id = "Heather_Lee_Grab_Fragaria_ananassa_USA_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
##############################################

data.species <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  filter(study_id=="conn02",sampling_year==2014)%>% 
  mutate(study_id = "Heather_Lee_Grab_Fragaria_ananassa_USA_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
##########################################

data.species <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  filter(study_id=="conn02",sampling_year==2015)%>% 
  mutate(study_id = "Heather_Lee_Grab_Fragaria_ananassa_USA_2015") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
###############################################

data.species <- read.xlsx("DATASETS/Taki01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  filter(sampling_year==2007)%>% 
  mutate(study_id = "Hisatomo_Taki_Fagopyrum_esculentum_Japan_2007") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
################################################

data.species <- read.xlsx("DATASETS/Taki01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2008)%>% 
  mutate(study_id = "Hisatomo_Taki_Fagopyrum_esculentum_Japan_2008") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
####################################################

data.species <- read.xlsx("DATASETS/Sche01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Jeroen_Scheper_Helianthus_annuus_France_2015") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#########################################################

data.species <- read.xlsx("DATASETS/Ekro01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- data.species[1:60,]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Johan_Ekroos_Vicia_faba_Sweden_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#########################################################

data.species <- read.xlsx("DATASETS/Hipo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2013) %>% 
  mutate(study_id = "Juliana_Hipólito_Coffea_arabica_Brazil_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#########################################################

data.species <- read.xlsx("DATASETS/Hipo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2014) %>% 
  mutate(study_id = "Juliana_Hipólito_Coffea_arabica_Brazil_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#########################################################

data.species <- read.xlsx("DATASETS/Sutt01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% separate(SiteID,c("SiteID","sub"),"-") %>% select(-sub)

data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year,
                                        sampling_method=Sampling_method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Louis_Sutter_Brassica_napus_Switzerland_2014") %>%
  rename(rank=Identified_to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#########################################################

data.species <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

names(data.species)[12] <- "NOTES_1"
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance_corrected= `Abundance.(corrected.for.differences.in.sampling.effort)`,
                                        Organism_ID=OrganismID,abundance_NO_corrected= `Abundance.(no.correction.for.sampling.effort)`)

data.species_01 <- data.species %>% 
  filter(grepl("carv02",data.species$site_id,ignore.case = TRUE)) %>% 
  mutate(study_id = "Luísa_G_Carvalheiro_Helianthus_annuus_South_Africa_2009") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

names(data.species)[12] <- "NOTES_1"
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance_corrected= `Abundance.(corrected.for.differences.in.sampling.effort)`,
                                        Organism_ID=OrganismID,abundance_NO_corrected= `Abundance.(no.correction.for.sampling.effort)`)

data.species_01 <- data.species %>%
  filter(grepl("carv01",data.species$site_id,ignore.case = TRUE)) %>% 
  mutate(study_id = "Luísa_G_Carvalheiro_Mangifera_indica_South_Africa_2008") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

names(data.species)[12] <- "NOTES_1"
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance_corrected= `Abundance.(corrected.for.differences.in.sampling.effort)`,
                                        Organism_ID=OrganismID,abundance_NO_corrected= `Abundance.(no.correction.for.sampling.effort)`)

data.species_01 <- data.species %>% 
  filter(grepl("carv03",data.species$site_id,ignore.case = TRUE)) %>% 
  mutate(study_id = "Luísa_G_Carvalheiro_Mangifera_indica_South_Africa_2009") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_04 <- data.species %>% filter(study_id=="garr04") %>% 
  mutate(study_id = "Michael_Garratt_Brassica_napus_UK_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_04)
#############################################


data.species <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_01 <- data.species %>% filter(study_id=="garr01") %>% 
  mutate(study_id = "Michael_Garratt_Fragaria_ananassa_UK_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_02 <- data.species %>% filter(study_id=="garr02") %>% 
  mutate(study_id = "Michael_Garratt_Malus_domestica_UK_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_02)
#############################################


data.species <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_03 <- data.species %>% filter(study_id=="garr03")%>% 
  mutate(study_id = "Michael_Garratt_Vicia_faba_UK_2011") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_03)
#############################################


data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(Year.of.sampling==2015)

# Be careful !! Abundance is given in counts, not no. visits × 15 min???1 × flower!!!
# No information about flower has been provided

# Evaluate the percentage of species + morphospecies
data.species %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species$Identified.to %in% c("morphospecies","species"))/nrow(data.species)



#We are going to estimate the number of census

data.meta <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                       sheet = "StudyMetadata", startRow = 2)

data.meta <- as_tibble(data.meta)

data.meta <- data.meta %>% filter(`Study.year(s)`==2015)

data.meta <- data.meta %>% select(Distance.ID,Number.of.visits.census)

data.species <- data.species %>% left_join(data.meta,by="Distance.ID")

data.species <- data.species %>% select(-Number.of.censuses)

data.species$Sampling.method <- paste(data.species$Number.of.visits.census,"census of 15 minutes observation to a flowering branch",sep=" ")


data.species <- data.species %>% rename(site_id=Distance.ID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID) %>% 
  mutate(study_id = "Natacha_Chacoff_Citrus_limon_Argentina_2015") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)
###########################################


data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(SiteID=="chac01",Year.of.sampling==2000)

# Be careful !! Abundance here reflects: no. visits × 15 min???1 × flower???1
# No information about flower has been provided


# Evaluate the percentage of species + morphospecies
data.species %>% group_by(Identified.to) %>% count()
percentage_species_morphos <- 
  sum(data.species$Identified.to %in% c("morphoespecies","species level"))/nrow(data.species)



#We are going to estimate the number of census

data.meta <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                       sheet = "StudyMetadata", startRow = 2)

data.meta <- as_tibble(data.meta)

data.meta <- data.meta %>% filter(StudyID=="chac01",`Study.year(s)`==2000)

data.meta <- data.meta %>% select(Distance.ID,Number.of.visits.census)

data.species <- data.species %>% left_join(data.meta,by="Distance.ID")

data.species <- data.species %>% select(-Number.of.censuses)

data.species$Sampling.method <- paste(data.species$Number.of.visits.census,"census of 15 minutes observation to a flowering branch",sep=" ")


data.species <- data.species %>% rename(site_id=Distance.ID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID) %>% 
  mutate(study_id = "Natacha_Chacoff_Citrus_paradisi_Argentina_2000") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)
###########################################

data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(SiteID=="chac01",Year.of.sampling==2001)

# Be careful !! Abundance here reflects: no. visits × 15 min???1 × flower???1
# No information about flower has been provided

# Evaluate the percentage of species + morphospecies
data.species %>% group_by(Identified.to) %>% count()
percentage_species_morphos <- 
  sum(data.species$Identified.to %in% c("morphoespecies","species level"))/nrow(data.species)


#We are going to estimate the number of census

data.meta <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                       sheet = "StudyMetadata", startRow = 2)

data.meta <- as_tibble(data.meta)

data.meta <- data.meta %>% filter(StudyID=="chac01",`Study.year(s)`==2001)

data.meta <- data.meta %>% select(Distance.ID,Number.of.visits.census)

data.species <- data.species %>% left_join(data.meta,by="Distance.ID")

data.species <- data.species %>% select(-Number.of.censuses)

data.species$Sampling.method <- paste(data.species$Number.of.visits.census,"censuses of 15 minutes observation to a flowering branch",sep=" ")


data.species <- data.species %>% rename(site_id=Distance.ID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)  %>% 
  mutate(study_id = "Natacha_Chacoff_Citrus_paradisi_Argentina_2001") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)
###########################################


data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(SiteID=="chac01",Year.of.sampling==2002)

# Be careful !! Abundance here reflects: no. visits × 15 min???1 × flower???1
# No information about flower has been provided

# Evaluate the percentage of species + morphospecies
data.species %>% group_by(Identified.to) %>% count()
percentage_species_morphos <- 
  sum(data.species$Identified.to %in% c("species level","morphoespecies"))/nrow(data.species)



#We are going to estimate the number of census

data.meta <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                       sheet = "StudyMetadata", startRow = 2)

data.meta <- as_tibble(data.meta)

data.meta <- data.meta %>% filter(StudyID=="chac01",`Study.year(s)`==2002)

data.meta <- data.meta %>% select(Distance.ID,Number.of.visits.census)

data.species <- data.species %>% left_join(data.meta,by="Distance.ID")

data.species <- data.species %>% select(-Number.of.censuses)

data.species$Sampling.method <- paste(data.species$Number.of.visits.census,"censuses of 15 minutes observation to a flowering branch",sep=" ")


data.species <- data.species %>% rename(site_id=Distance.ID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)  %>% 
  mutate(study_id = "Natacha_Chacoff_Citrus_paradisi_Argentina_2002") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)

##########################################

data.species <- read.xlsx("DATASETS/Cavi01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)  %>% 
  mutate(study_id = "Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species)

#############################################

data.species <- read.xlsx("DATASETS/Mall01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2012) %>% 
  mutate(study_id = "Rachel_Mallinger_Malus_domestica_USA_2012") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Mall01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2013) %>% 
  mutate(study_id = "Rachel_Mallinger_Malus_domestica_USA_2013") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Stew01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Rebecca_Steward_Fragaria_ananassa_Sweden_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Bomm01_Datacollection_pollination.xlsx",
                         sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Riccardo_Bommarco_Brassica_napus_Sweden_2005") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Radewill01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2014) %>% 
  mutate(study_id = "Romina_Rader_Mangifera_indica_Australia_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Cuss01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Sarah_Cusser_Gossypium_hirsutum_USA_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Pott01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_01<- data.species %>% 
  mutate(study_id = "Simon_Potts_Vicia_faba_UK_2005") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Krish01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2007) %>% 
  mutate(study_id = "Smitha_Krishnan_Coffea_canephora_India_2007") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Krish01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2008) %>% 
  mutate(study_id = "Smitha_Krishnan_Coffea_canephora_India_2008") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Krish01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2009) %>% 
  mutate(study_id = "Smitha_Krishnan_Coffea_canephora_India_2009") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Krish02_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2014) %>% 
  mutate(study_id = "Smitha_Krishnan_Coffea_canephora_India_2014") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Fije01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>%
  separate(col = SiteID, into = c("study_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])",remove = F)

data.species <- data.species %>% select(-dis)
data.species$study_id[data.species$study_id == "IT"] <- "fije02"
data.species$study_id[data.species$study_id == "FR"] <- "fije01"

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(study_id=="fije01") %>% 
  mutate(study_id = "Thijs_Fijen_Allium_porrum_France_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data.species <- read.xlsx("DATASETS/Fije01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>%
  separate(col = SiteID, into = c("study_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])",remove = F)

data.species <- data.species %>% select(-dis)
data.species$study_id[data.species$study_id == "IT"] <- "fije02"
data.species$study_id[data.species$study_id == "FR"] <- "fije01"

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(study_id=="fije02") %>% 
  mutate(study_id = "Thijs_Fijen_Allium_porrum_Italy_2016") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################


data.species <- read.xlsx("DATASETS/Bore01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% mutate(Abundance=round(`Abundance.(mean.abundance.for.15min.obs.and.30.inflorescences)`*Number.of.censuses))
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)
data.species <- data.species %>% mutate(total_time=10*15*Number.of.censuses)

data.species_04 <- data.species %>% 
  mutate(study_id = "Virginie_Boreux_Coffea_canephora_India_2008") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_04)
#############################################


data.species <- read.xlsx("DATASETS/Bore02_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_04 <- data.species %>% 
  mutate(study_id = "Virginie_Boreux_Malus_domestica_Germany_2015") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_04)
#############################################


data.species <- read.xlsx("DATASETS/Zouy01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% 
  mutate(study_id = "Yi_Zou_Brassica_napus_China_2015") %>%
  rename(rank=Identified.to) %>%
  select(study_id,sampling_method,Organism_ID,rank) %>% unique()

data_dainese <- bind_rows(data_dainese,data.species_01)
#############################################

data_dainese <- unique(data_dainese) %>% filter(!is.na(Organism_ID)) %>%
  filter(Organism_ID!="0")

write_csv(data_dainese,"taxon_DAINESE.csv")
