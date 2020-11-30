
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()

##########################
#Data: DAINESE, Frei01: FREI03
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Frei01_Datacollection_pollination.xlsx",
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

data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))


data.Final_Services <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Frei01_Datacollection_pollination.xlsx",
                              sheet = "Final Ecosystem Services", startRow = 2)
data.Final_Services <- as_tibble(data.Final_Services)
data.Final_Services$Crop.yield <- as.numeric(data.Final_Services$Crop.yield)

data.Final_Services <- data.Final_Services %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,
         yield2=Crop.yield, yield2_units= Unit)

data.site <- data.site %>% left_join(data.Final_Services, by = c("site_id","sampling_year"))

data.LandscapeData <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Frei01_Datacollection_pollination.xlsx",
                                 sheet = "LandscapeData", startRow = 2)
data.LandscapeData <- as_tibble(data.LandscapeData)
data.LandscapeData$X <- as.numeric(data.LandscapeData$X)
data.LandscapeData$Y <- as.numeric(data.LandscapeData$Y)
data.LandscapeData$Field.size <- as.numeric(data.LandscapeData$Field.size)

data.LandscapeData <- data.LandscapeData %>% select(SiteID,X,Y,Field.size)%>%
  rename(site_id=SiteID,X_UTM=X,Y_UTM=Y,field_size=Field.size)

data.site <- data.site %>% left_join(data.LandscapeData, by = c("site_id","X_UTM","Y_UTM"))
data.site <- data.site %>% mutate(country="Brazil",Publication="10.1073/pnas.1517092112",
                                  Credit="Breno M. Freitas",email="freitas@ufc.br")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Frei01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_03 <- data.species %>% filter(sampling_year==2011,site_id %in% data.site$site_id)

# Evaluate the percentage of species + morphospecies
data.species_03 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_03$Identified.to %in% c("species"))/nrow(data.species_03)

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_03 %>% select(-Identified.to,-X6)

data.species_03 <- data.species_03 %>% left_join(gild_list,by=c("Organism_ID","Family"))

data.species_03 %>% filter(is.na(Guild))%>% count()

data.species_03 <- data.species_03 %>% mutate(total_sampled_area=Number.of.censuses*150,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="6 subplots per site,	150 m, 4 sampling rounds in one season")


insect_sampling <- tibble(
  study_id = "Breno_M_Freitas_Malpighia_emarginata_Brazil_2011",
  site_id = data.species_03$site_id,
  pollinator = data.species_03$Organism_ID,
  guild = data.species_03$Guild,
  sampling_method = data.species_03$sampling_method,
  abundance = data.species_03$abundance,
  total_sampled_area = data.species_03$total_sampled_area,
  total_sampled_time = data.species_03$total_sampled_time,
  total_sampled_flowers = data.species_03$total_sampled_flowers,
  Description = data.species_03$Description
)

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Breno_M_Freitas_Malpighia_emarginata_Brazil_2011.csv")

# setwd(dir_ini)

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_03 %>% group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

# There are only other_wild_bees

abundance_aux <- abundance_aux %>% mutate(honeybees=0,bumblebees=0,syrphids=0,humbleflies=0,
                                  other_flies=0,
                                  non_bee_hymenoptera=0,lepidoptera=0,beetles=0,other=0,
                                  total=other_wild_bees)


######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_03 %>% select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator
}

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos<0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}



###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="Breno_M_Freitas_Malpighia_emarginata_Brazil_2011",
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=NA,
  management=data.site$management,
  country=data.site$country,
  latitude=NA,
  longitude=NA,
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=NA,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield2,
  yield_units=data.site$yield2_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=richness_aux$observed_pollinator_richness,
  other_pollinator_richness=richness_aux$other_pollinator_richness,
  other_richness_estimator_method=richness_aux$other_richness_estimator_method,
  abundance=abundance_aux$total,
  ab_honeybee=abundance_aux$honeybees,
  ab_bombus=abundance_aux$bumblebees,
  ab_wildbees=abundance_aux$other_wild_bees,
  ab_syrphids=abundance_aux$syrphids,
  ab_humbleflies=abundance_aux$humbleflies,
  ab_other_flies=abundance_aux$other_flies,
  ab_beetles=abundance_aux$beetles,
  ab_lepidoptera=abundance_aux$lepidoptera,
  ab_nonbee_hymenoptera=abundance_aux$non_bee_hymenoptera,
  ab_others=abundance_aux$other,
  total_sampled_area=600,
  total_sampled_time=NA,
  visitation_rate_units = NA,
  visitation_rate=NA,
  visit_honeybee=NA,
  visit_bombus=NA,
  visit_wildbees=NA,
  visit_syrphids=NA,
  visit_humbleflies=NA,
  visit_other_flies=NA,
  visit_beetles=NA,
  visit_lepidoptera=NA,
  visit_nonbee_hymenoptera=NA,
  visit_others=NA,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)
# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Breno_M_Freitas_Malpighia_emarginata_Brazil_2011.csv")
# setwd(dir_ini)

##########################
# UPDATE
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/field_level_data_Breno_M_Freitas_Malpighia_emarginata_Brazil_2011_corrected.xlsx",
                       sheet = "field_level_data_Breno_M_Freita", startRow = 1)
data.site <- as_tibble(data.site)

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data.site, "Processing_files/Datasets_storage/field_level_data_Breno_M_Freitas_Malpighia_emarginata_Brazil_2011.csv")
# setwd(dir_ini)
