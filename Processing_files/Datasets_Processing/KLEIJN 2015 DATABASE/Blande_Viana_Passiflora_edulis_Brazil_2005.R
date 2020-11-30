
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 89_BlandeViana_PassionFruit2005
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/89_BlandeViana_PassionFruit2005/Copy of Mango_Passion_Fruit_Blande_David_Bees_Abundance.xls",
                       sheet = "PassionFruit")

# Select sampling data
col_names <- data_raw[2,1:11]
data_raw <- data_raw[3:18,1:11]
colnames(data_raw) <- col_names

data_raw <- as_tibble(data_raw)

# Fix coordinates
library(raster)
library(rgdal)
epsg <- make_EPSG()
crsWGS84    = epsg %>% filter(code == 4326)
crsSAD69_24 = epsg %>% filter(code == 5534)
selPoints = SpatialPoints(cbind(as.numeric(data_raw$X),as.numeric(data_raw$Y)),
                          proj4string=CRS(crsSAD69_24$prj4))
projSelPts <- spTransform(selPoints, CRS(crsWGS84$prj4))


data_raw$longitude <- NA
data_raw$latitude <- NA
data_raw[,12:13] <- projSelPts@coords


# Adapt headers

data_raw <- data_raw %>% rename(crop=Crop,sampling_year=Year,X_UTM=X,Y_UTM=Y,site_id=site)


# There should be 16 sites

data_raw %>% group_by(site_id) %>% count()

##############
# Data site
##############


data.site <- data_raw %>% dplyr::select(crop,sampling_year,X_UTM,Y_UTM,site_id,
                                 latitude,longitude)

data.site$study_id <- "Blande_Viana_Passiflora_edulis_Brazil_2005"
data.site$crop <- "Passiflora edulis"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Brazil"
data.site$zone_UTM <- 24
data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA
data.site$field_size <- NA
data.site$yield <- NA
data.site$yield_units <- NA
data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA
data.site$Publication <- "10.1038/ncomms8414"
data.site$Credit <- "Blande Viana"
data.site$Email_contact <- "blande.viana@gmail.com"



###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  dplyr::select(site_id,
                'Total number of Xylocopa bees per a 50m transect (~90 flowers in a trasect of 50m)',
                'Total number of Trigona spinipes bees per a 50m transect (~90 flowers in a trasect of 50m)',
                'Total number of Apis mellifera per a 50m transect(~90 flowers in a trasect of 50m)')

# Adapt headers
colnames(data_raw_obs) <- c("site_id","Xylocopa spp.","Apis mellifera","Trigona spinipes")

# Rearrange species

data_raw_obs <- data_raw_obs %>%
  gather("Organism_ID","abundance",
         c("Xylocopa spp.","Apis mellifera","Trigona spinipes")) %>%
  filter(abundance>0)

data_raw_obs$abundance <- as.numeric(data_raw_obs$abundance)

#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% dplyr::select(-Family) %>% unique()

list_organisms <- dplyr::select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Fix Guild
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Xylocopa spp."] <- "other_wild_bees"

#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

insect_sampling <- tibble(
  study_id = "Blande_Viana_Passiflora_edulis_Brazil_2005",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "transects",
  abundance = 9*as.numeric(data_obs_guild$abundance),
  total_sampled_area = 3*3*50,
  total_sampled_time = 3*3*15,
  total_sampled_flowers = 3*3*90,
  Description = "In each field, bees were surveyed in a 50 m long transect, laid within the crop field, with a mean of 90 flowers observed for 15 minutes during three times on three different days"
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Blande_Viana_Passiflora_edulis_Brazil_2005.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE) %>%
  mutate(abundance=9*abundance)

# Remove entries with unknow guilds
data_obs_guild_2  <- data_obs_guild_2  %>% filter(!is.na(Guild),Guild!="NA")

abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "other_wild_bees"     "honeybees"


# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          bumblebees=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
  dplyr::select(site_id,Organism_ID,abundance)%>%mutate(abundance=9*abundance)%>%
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

# Load our estimation for taxonomic resolution
percentage_species_morphos <- 0.5

richness_aux <- abundace_field %>% dplyr::select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="only bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = data.site$study_id,
  site_id = data.site$site_id,
  crop = data.site$crop,
  variety = data.site$variety,
  management = data.site$management,
  country = data.site$country,
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=data.site$zone_UTM,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_no_pollinators,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  richness_restriction = data.site$richness_restriction,
  abundance = data.site$total,
  ab_honeybee = data.site$honeybees,
  ab_bombus = data.site$bumblebees,
  ab_wildbees = data.site$other_wild_bees,
  ab_syrphids = data.site$syrphids,
  ab_humbleflies= data.site$humbleflies,
  ab_other_flies= data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others = data.site$other,
  total_sampled_area = 9*50,
  total_sampled_time = 9*15,
  visitation_rate_units = "counts per flower and hour",
  visitation_rate = data.site$total*60/(90*3*3*15*3*3),
  visit_honeybee = data.site$honeybees*60/(90*3*3*15*3*3),
  visit_bombus = data.site$bumblebees*60/(90*3*3*15*3*3),
  visit_wildbees = data.site$other_wild_bees*60/(90*3*3*15*3*3),
  visit_syrphids = data.site$syrphids*60/(90*3*3*15*3*3),
  visit_humbleflies = data.site$humbleflies*60/(90*3*3*15*3*3),
  visit_other_flies = data.site$other_flies*60/(90*3*3*15*3*3),
  visit_beetles = data.site$beetles*60/(90*3*3*15*3*3),
  visit_lepidoptera = data.site$lepidoptera*60/(90*3*3*15*3*3),
  visit_nonbee_hymenoptera = data.site$non_bee_hymenoptera*60/(90*3*3*15*3*3),
  visit_others = data.site$other*60/(90*3*3*15*3*3),
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Blande_Viana_Passiflora_edulis_Brazil_2005.csv")
#setwd(dir_ini)

