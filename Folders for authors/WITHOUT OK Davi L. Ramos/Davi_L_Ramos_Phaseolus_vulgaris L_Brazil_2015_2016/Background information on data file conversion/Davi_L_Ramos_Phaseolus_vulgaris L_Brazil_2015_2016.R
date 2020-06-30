
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Ramo01: ramo01
##########################


data.site <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,latitude=X,longitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.site$study_id <- "Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016"

data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD

data.Final <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                              sheet = "Final Ecosystem Services", startRow = 3)

data.Final <- as_tibble(data.Final)

data.Final <- data.Final %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,yield=Crop.yield,yield_units=Unit)

#Adapting units
data.Final$yield <- 10*data.Final$yield 
data.Final$yield_units <- "kg/hectare"

data.site <- data.site %>% left_join(data.Final,by=c("site_id","sampling_year"))
  
# MORE YIELD

data.Functioning <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,
         yield2=Function,yield2_units=Type.of.function)%>%
  select(-Exclosure.treatment)

data.site <- data.site %>% left_join(data.Functioning,by=c("site_id","sampling_year"))
#######################################################################################
#######################################################################################
###########################
# Adding  Field_size

data.LandscapeData <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                                 sheet = "LandscapeData", startRow = 2)
data.LandscapeData <- as_tibble(data.LandscapeData)
#data.LandscapeData$X <- as.numeric(data.LandscapeData$X)
#data.LandscapeData$Y <- as.numeric(data.LandscapeData$Y)
#data.LandscapeData$Field.size <- as.numeric(data.LandscapeData$Field.size)

data.LandscapeData <- data.LandscapeData %>% select(SiteID,Field.size)%>%
  rename(site_id=SiteID,field_size=Field.size)

data.site <- data.site %>% left_join(data.LandscapeData, by = c("site_id"))
data.site <- data.site %>% mutate(country="Brazil",Publication=NA,
                                  Credit="Davi L. Ramos; Felipe D. da Silva e Silva (University of Brasilia)",
                                  email="davilramos91@gmail.com")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Ramo01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 4)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance.not.corrected.for.sampling.effort,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(!abundance==0.01)

# Evaluate the percentage of species + morphospecies
data.species_01 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_01$Identified.to %in% c("species","morphospecies"))/nrow(data.species_01)

data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to,-X6,-X7)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Bombus sp. "] <- "bumblebees"


data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=50,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=abundance/Corrected.for.sampling.effort,
                                              Description="Spatial replicates: two 25m observation transects per plot	2. Temporal replicates: 2Lumped sampling rounds per plot in one wet season ") 

aditional_data <- data.species_01 %>% group_by(site_id) %>%summarise(total_sampled_area=mean(total_sampled_area),
                                                   total_sampled_flowers=mean(total_sampled_flowers))

data.site <- data.site %>% left_join(aditional_data,by="site_id")

insect_sampling <- tibble(
  study_id = "Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016",
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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_01 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are bumblebees","honeybees",non_bee_hymenoptera"
#other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,other=0,
                                          other_flies=0,humbleflies=0,total=0)
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
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016",
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
  total_sampled_area=data.site$total_sampled_area,
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
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv")
setwd(dir_ini)
