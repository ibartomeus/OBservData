
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Grab01: conn02
##########################


data.site <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(`StudyID?`=="conn02")
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)
data.site$Year <- 2014

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=`StudyID?`,site_id=SiteID,X_UTM=X,Y_UTM=Y,zone_UTM=Zone,
         sampling_year=Year,crop=Crop.species,
         management=Management)

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=18 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

data.site$longitude <- NA
data.site$latitude <- NA
data.site[,9:10] <- spgeo@coords

#####################################
# YIELD
# Note that there are three treatments: Closed, supplementary, open

data.Final <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                                 sheet = "Final Ecosystem Services", startRow = 2)

data.Final <- as_tibble(data.Final)

data.Final <- data.Final %>% filter(StudyID=="conn02",Year.of.sampling==2014) %>% select(-StudyID)

data.Final <- data.Final %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

data.Final <- data.Final %>% spread(X7,Crop.yield)

data.Final <- data.Final %>% rename(yield=open,
                                    yield_treatments_pollen_supplement=hand) %>%
  select(-Unit,-X6) %>% mutate(yield_units="grams (average fruit weight)")
                                    
#data.site$sampling_year <- as.numeric(data.site$sampling_year)

data.site <- data.site %>% left_join(data.Final, by=c("site_id","sampling_year"))

######################################################################################
#YIELD 2

data.Functioning <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                        sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(StudyID=="conn02",Year.of.sampling==2014) %>% select(-StudyID)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

data.Functioning <- data.Functioning %>% spread(Exclosure.treatment,Function)

data.Functioning <- data.Functioning %>% rename(yield2=open,
                                    yield_treatments_pollen_supplement2=hand,
                                    yield2_units=Type.of.function)

#data.site$sampling_year <- as.numeric(data.site$sampling_year)

data.site <- data.site %>% left_join(data.Functioning, by=c("site_id","sampling_year"))



#######################################################################################
#######################################################################################
# Adding  Field_size

data.site$field_size <- 0.005 #50m2 experimental plots

data.site <- data.site %>% mutate(country="USA",Publication="NA",
                                  Credit="USDA NASS",
                                  email="hlc66@cornell.edu")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Grab01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(study_id=="conn02",sampling_year==2014)
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("../Tesauro_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to,-X7)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01 <- data.species_01 %>% filter(!is.na(Organism_ID))


data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="Temporal replicates: 3-4X per season") 


insect_sampling <- tibble(
  study_id = "grab012",
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
write_csv(insect_sampling, "insect_sampling_grab012.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################
data.species_01 %>% group_by(sampling_method) %>% count()

abundance_aux <- data.species_01 %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "honeybees","other_wild_bees","bumblebees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,other_flies=0,
                                          non_bee_hymenoptera=0,other=0,humbleflies=0,syrphids=0,
                                          total=0)
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

richness_aux <- abundace_field %>% select(site_id, r_chao)
richness_aux <- richness_aux %>% rename(pollinator_richness=r_chao) %>%
  mutate(richness_estimator_method="Chao1")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="grab012",
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=NA,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=data.site$zone_UTM,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=NA,
  fruit_weight="see yield",
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  pollinator_richness=data.site$pollinator_richness,
  richness_estimator_method=data.site$richness_estimator_method,
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
write_csv(field_level_data, "field_level_data_grab012.csv")
setwd(dir_ini)
