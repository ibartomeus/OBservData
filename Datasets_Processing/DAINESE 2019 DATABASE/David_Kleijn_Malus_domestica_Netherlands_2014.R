
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Groo01: Groo02
##########################


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

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
 
# Adding crop yield

data.Final_Services <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                              sheet = "Final Ecosystem Services", startRow = 2)
data.Final_Services <- as_tibble(data.Final_Services)
data.Final_Services$Crop.yield <- as.numeric(data.Final_Services$Crop.yield)

data.Final_Services <- data.Final_Services %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,
         yield=Crop.yield, yield_units= Unit)

data.site <- data.site %>% left_join(data.Final_Services, by = c("site_id","sampling_year"))


# Adding fruitset
# Note that there are three treatments: Closed, supplementary, open

data.Functioning <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

data.Functioning <- data.Functioning %>% spread(Exclosure.treatment,Function)

data.Functioning <- data.Functioning %>%
  rename(yield_treatments_no_pollinators2=Closed,
         yield2=Open,
         yield_treatments_pollen_supplement2=Supplimentary,
         yield2_units=Type.of.function)

data.site <- data.site %>% left_join(data.Functioning, by = c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.LandscapeData <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                                 sheet = "LandscapeData", startRow = 2)
data.LandscapeData <- as_tibble(data.LandscapeData)
data.LandscapeData$X <- as.numeric(data.LandscapeData$X)
data.LandscapeData$Y <- as.numeric(data.LandscapeData$Y)
data.LandscapeData$Field.size <- as.numeric(data.LandscapeData$Field.size)

data.LandscapeData <- data.LandscapeData %>% select(SiteID,X,Y,Field.size)%>%
  rename(site_id=SiteID,X_UTM=X,Y_UTM=Y,field_size=Field.size)

# We assume that rows/fields from 16 to 30 correspond to Groo02
#data.LandscapeData <- unique(data.LandscapeData)

data.LandscapeData <- data.LandscapeData[16:30,c(1,4)]

data.site <- data.site %>% left_join(data.LandscapeData, by = c("site_id"))
data.site <- data.site %>% mutate(country="Netherlands",Publication="De Groot et al. 2015, Alterra report 2636 (Dutch)",
                                  Credit="G.A. (Arjen) de Groot, Wageningen Environmental Research (Alterra) + David Kleijn, Wageningen University",
                                  email="g.a.degroot@wur.nl")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Groo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

#Somehow three columns [7,8,9] appear repeated-> FIXING PROBLEM:
data.species <- data.species[-c(7,8,9)]

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_02 <- data.species %>% filter(sampling_year==2014,site_id %in% data.site$site_id)

# Evaluate the percentage of species + morphospecies
data.species_02 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_02$Identified.to %in% c("Species"))/nrow(data.species_02)

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_02 %>% select(-Identified.to,-X6)


data.species_02 <- data.species_02 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_02 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears due to spaces in the excel entries > FIX
#data.species_03$Guild[is.na(data.species_03$Guild)] <- "other_wild_bees"
#data.species_03 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_02 <- data.species_02 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="within one crop field, 3 plots for crop measurements and 12 inventory transects were randomly located.	2 inventory rounds per transect (1x morning, 1x afternoon)") 


insect_sampling <- tibble(
  study_id = "David_Kleijn_Malus_domestica_Netherlands_2014",
  site_id = data.species_02$site_id,
  pollinator = data.species_02$Organism_ID,
  guild = data.species_02$Guild,
  sampling_method = data.species_02$sampling_method,
  abundance = data.species_02$abundance,
  total_sampled_area = data.species_02$total_sampled_area,
  total_sampled_time = data.species_02$total_sampled_time,
  total_sampled_flowers = data.species_02$total_sampled_flowers,
  Description = data.species_02$Description
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_David_Kleijn_Malus_domestica_Netherlands_2014.csv")

setwd(dir_ini)
                               
#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_02 %>% group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

# There are only bumblebees, honeybees,other_wild_bees,syrphids
# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,non_bee_himenoptera=0,humbleflies=0,
                                        other_flies=0,beetles=0,other=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_02 %>% select(site_id,Organism_ID,abundance)%>%
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
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="David_Kleijn_Malus_domestica_Netherlands_2014",
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=NA,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
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
  ab_nonbee_hymenoptera=data.site$non_bee_himenoptera,
  ab_others=data.site$other,
  total_sampled_area=NA,
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

# Adding David corrections
field_level_data_mod <- read_csv("DATASETS/field_level_data_David_Kleijn_Malus_domestica_Netherlands_2014.csv")
field_level_data$variety <- field_level_data_mod$variety
field_level_data$yield_units <- field_level_data_mod$yield_units


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_David_Kleijn_Malus_domestica_Netherlands_2014.csv")
setwd(dir_ini)
