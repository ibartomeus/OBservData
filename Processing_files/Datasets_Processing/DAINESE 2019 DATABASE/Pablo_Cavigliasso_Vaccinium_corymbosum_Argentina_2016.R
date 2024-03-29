
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)
library(taxize)

dir_ini <- getwd()

##########################
#Data: DAINESE, Cavi01: cavi01
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Cavi01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site$study_id <- "Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016"
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

#data.site$management <- unname(management_types[data.site$management])
data.site <- data.site %>% separate(crop,c("crop","variety"),". Cultivar: ")


##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD

data.Functioning <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Cavi01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

yield_aux <- data.Functioning
yield_aux$Exclosure.treatment <- "fruitset (%)"

yield_aux$Fruit.set.Open.pollination <- 100*yield_aux$Fruit.set.Open.pollination
yield_aux$Fruit.set.Exclosure <- 100*yield_aux$Fruit.set.Exclosure

yield_aux <- yield_aux %>%
  rename(site_id=SiteID,
         sampling_year=Year.of.sampling,
         yield_treatments_no_pollinators=Fruit.set.Exclosure,
         yield=Fruit.set.Open.pollination,
         yield_units=Exclosure.treatment)

data.site <- data.site %>% left_join(yield_aux, by = c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.LandscapeData <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Cavi01_Datacollection_pollination.xlsx",
                                 sheet = "LandscapeData", startRow = 2)
data.LandscapeData <- as_tibble(data.LandscapeData)
#data.LandscapeData$X <- as.numeric(data.LandscapeData$X)
#data.LandscapeData$Y <- as.numeric(data.LandscapeData$Y)
#data.LandscapeData$Field.size <- as.numeric(data.LandscapeData$Field.size)

data.LandscapeData <- data.LandscapeData %>% select(SiteID,Field.size)%>%
  rename(site_id=SiteID,field_size=Field.size)

data.site <- data.site %>% left_join(data.LandscapeData, by = "site_id")
data.site <- data.site %>% mutate(country="Argentina",Publication=NA,
                                  Credit="Pablo Cavigliasso. Instituto Nacional de Tecnologia Agropecuaria (INTA) - Programa Nacional Apicola (PNAPI)",
                                  email="pablo.cavigliaso@gmail.com; cavigliasso.pablo@inta.gob.ar")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Cavi01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species

#####################################
# Evaluate the percentage of species + morphospecies
data.species_01 %>% group_by(Identified.to) %>% count()

# No taxonomic resolution has been provided.
# Load our estimation for taxonomic resolution

tax_res <- read_csv("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/taxon_table_cavi01.csv") %>% rename(Organism_ID=pollinator)
tax_estimation <- data.species_01 %>% left_join(tax_res, by="Organism_ID")
tax_estimation %>% group_by(rank) %>% count()

percentage_species_morphos <-
  sum(tax_estimation$rank %in% c("morphospecies","species"))/nrow(tax_estimation)

######################

data.species_01 %>% group_by(sampling_method) %>% count()
data.species_01$sampling_method <- "Census of 5 minutes"

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_01 <- data.species_01 %>% select(-Identified.to)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Sarcophagidae "] <- "other_flies"

data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=30*5,
                                              total_sampled_flowers=NA,
                                              Description="Observation: 5 minutes per plant, 6 plant observations  per site, and	5 rounds per site in season")


insect_sampling <- tibble(
  study_id = "Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016",
  site_id = data.species_01$site_id,
  pollinator = data.species_01$Organism_ID,
  guild = data.species_01$Guild,
  sampling_method = data.species_01$sampling_method,
  abundance = data.species_01$abundance,
  total_sampled_area = data.species_01$total_sampled_area,
  total_sampled_time = data.species_01$total_sampled_time,
  total_sampled_flowers = data.species_01$total_sampled_flowers,
  Description = data.species_01$Description
)

# Fix bombus belicosus

insect_sampling$pollinator[insect_sampling$pollinator=="Bombus belicoso"] <-
  "Bombus bellicosus"


# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016.csv")

# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_01 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles","bumblebees","honeybees","lepidoptera","non_bee_hymenoptera",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>%
  select(site_id,Organism_ID,abundance)%>%
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

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

#######################################################################
#######################################################################
#VISITATION RATE

visit_aux <- abundance_aux

visit_aux[,2:ncol(visit_aux)] <- 60*visit_aux[,2:ncol(visit_aux)]/(30*150)

visit_aux <- visit_aux %>% rename(

  visitation_rate= total,
  visit_honeybee= honeybees,
  visit_bombus= bumblebees,
  visit_wildbees= other_wild_bees,
  visit_syrphids= syrphids,
  visit_humbleflies= humbleflies,
  visit_other_flies= other_flies,
  visit_beetles= beetles,
  visit_lepidoptera= lepidoptera,
  visit_nonbee_hymenoptera= non_bee_hymenoptera,
  visit_others= other,
)
data.site <- data.site %>% left_join(visit_aux, by = "site_id")
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id=data.site$study_id,
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=data.site$variety,
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
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  abundance=data.site$total,
  ab_honeybee=data.site$honeybees,
  ab_bombus=data.site$bumblebees,
  ab_wildbees=data.site$other_wild_bees,
  ab_syrphids=data.site$syrphids,
  ab_humbleflies=data.site$humbleflies,
  ab_other_flies=data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others=data.site$other,
  total_sampled_area=NA,
  total_sampled_time=30*5,
  visitation_rate_units="visits per hour and plant",
  visitation_rate=data.site$visitation_rate,
  visit_honeybee=data.site$visit_honeybee,
  visit_bombus=data.site$visit_bombus,
  visit_wildbees=data.site$visit_wildbees,
  visit_syrphids=data.site$visit_syrphids,
  visit_humbleflies=data.site$visit_humbleflies,
  visit_other_flies=data.site$visit_other_flies,
  visit_beetles=data.site$visit_beetles,
  visit_lepidoptera=data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera=data.site$visit_nonbee_hymenoptera,
  visit_others=data.site$visit_others,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)

#############
# Fixing latitude and longitude

field_level_data_aux <- read_csv("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/field_level_data_Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016_REV.csv")

field_level_data$latitude <- field_level_data_aux$latitude
field_level_data$longitude <- field_level_data_aux$longitude

# Fixing sampling months
field_level_data$sampling_start_month <- field_level_data_aux$sampling_start_month
field_level_data$sampling_end_month <- field_level_data_aux$sampling_end_month


# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Pablo_Cavigliasso_Vaccinium_corymbosum_Argentina_2016.csv")
# setwd(dir_ini)
