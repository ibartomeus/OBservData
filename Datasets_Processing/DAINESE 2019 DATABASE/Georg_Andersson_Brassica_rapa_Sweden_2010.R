
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()

##########################
#Data: DAINESE, Ande01: ande01
##########################


data.site <- read.xlsx("DATASETS/Ande01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Year==2010)
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.site$management <- unname(management_types[data.site$management])

data.site$study_id <- "Georg_Andersson_Brassica_rapa_Sweden_2010"

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD
# Note that there are three treatments: Closed, supplementary, open

data.Functioning <- read.xlsx("DATASETS/Ande01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(Year.of.sampling==2010)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling) %>% select(-Exclosure.treatment)

yield_aux <- data.Functioning %>%
  rename(yield=Function,
         yield_units=Type.of.function)

# Change to percentage
yield_aux$yield <- 100*yield_aux$yield
yield_aux$yield_units <- "seed set (Percentage developed seed per siliqua)"	
data.site <- data.site %>% left_join(yield_aux, by = c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.site$field_size <- NA
data.site <- data.site %>% mutate(country="Sweden",Publication=NA,
                                  Credit="Georg Andersson (Lund University), Project MULTIFUNC",
                                  email="georg.andersson@cec.lu.se")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Ande01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)



data.species_01 <- data.species %>% filter(sampling_year==2010)

# Evaluate the percentage of species + morphospecies

data.species_01 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <- sum(data.species_01$Identified.to %in% c("Species"))/nrow(data.species_01)


# fixing site_id: Wrong codification
data.species_01$site_id <- as.character(round(as.numeric(data.species_01$site_id),2))
unique(data.species_01$site_id)

unique(data.species_01$site_id[!data.species_01$site_id%in% data.site$site_id])

new_labels <- c("1.1"="1.1","2.01"="2.0.1", "2.02"="2.0.2", "2.1"="2.1","2.4"="2.4",
                "2.42"="2.4.2","4.4"="4.4", "5.4"="5.4",
                "4.01"="4.1","4.1"="4.1", "4.12"="4.1.2", "5.01"="5.0.1")

data.species_01$site_id <- unname(new_labels[data.species_01$site_id])

#Testing site labels
unique(data.species_01$site_id[!data.species_01$site_id%in% data.site$site_id])
data.species_01 %>% filter(is.na(site_id))

data.species_01 %>% group_by(site_id) %>% count()
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to,-X6)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Cerceris rybyensis "] <- "non_bee_hymenoptera"
data.species_01$Guild[data.species_01$Organism_ID=="Lasioglossum minutissimum "] <- "other_wild_bees"
data.species_01$Guild[data.species_01$Organism_ID=="Trypoxylon clavicerum "] <- "non_bee_hymenoptera"

data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="4 pots with turnip plants per site, and	6 temporal replicates") 


insect_sampling <- tibble(
  study_id = "Georg_Andersson_Brassica_rapa_Sweden_2010",
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
write_csv(insect_sampling, "insect_sampling_Georg_Andersson_Brassica_rapa_Sweden_2010.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_01 %>% filter(sampling_method=="Sweepnets") %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "","bumblebees","honeybees","","non_bee_hymenoptera","",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,other=0, humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>% filter(sampling_method=="Sweepnets") %>%
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
# VISITATION RATE

raw_visits <- read_csv("DATASETS/rybsObs.csv") %>% 
  filter(!is.na(Time),!is.na(observationtime_min)) %>%
  mutate(syrphids=Syrphids_hairy+Syrphids_bald,
         bumblebees=`Bumblebees_Long-toungued`+`Bumblebees_short-toungued`) %>%
  rename(site_id=Landscape_ID)

raw_visits %>% group_by(Landskap) %>% count()
  
# sanity check

raw_visits$site_id[!raw_visits$site_id %in% data.site$site_id] %>% unique()

raw_visits$site_id[raw_visits$site_id=="2.42"] <- "2.4.2"
raw_visits$site_id[raw_visits$site_id=="4.01"] <- "4.1"
raw_visits$site_id[raw_visits$site_id=="2.01"] <- "2.0.1"
raw_visits$site_id[raw_visits$site_id=="4.12"] <- "4.1.2"
raw_visits$site_id[raw_visits$site_id=="5.01"] <- "5.0.1"
raw_visits$site_id[raw_visits$site_id=="2.02"] <- "2.0.2"

raw_visits$site_id[!raw_visits$site_id %in% data.site$site_id] %>% unique()
  
visits_aux <- raw_visits %>% select(site_id,observationtime_min,Number_flowers,
                      Honeybees,bumblebees,Wild_bees,
                      syrphids,Other_diptera,Other_Hymenoptera,total_visits) %>%
  group_by(site_id) %>% summarise_all(sum,na.rm = TRUE)

visits_final <- visits_aux %>%
  mutate(visitation_rate=60*100*total_visits/observationtime_min/Number_flowers,
         visit_honeybee=60*100*Honeybees/observationtime_min/Number_flowers,
         visit_bombus=60*100*bumblebees/observationtime_min/Number_flowers,
         visit_wildbees=60*100*Wild_bees/observationtime_min/Number_flowers,
         visit_syrphids=60*100*syrphids/observationtime_min/Number_flowers,
         visit_humbleflies=0,
         visit_other_flies=60*100*Other_diptera/observationtime_min/Number_flowers,
         visit_beetles=0,
         visit_lepidoptera=0,
         visit_nonbee_hymenoptera=60*100*Other_Hymenoptera/observationtime_min/Number_flowers,
         visit_others=0) %>%
  select(site_id,observationtime_min,
    visitation_rate,visit_honeybee,visit_bombus,
         visit_wildbees,visit_syrphids,visit_humbleflies,
         visit_other_flies,visit_beetles,visit_lepidoptera,
         visit_nonbee_hymenoptera,visit_others)

data.site <- data.site %>% left_join(visits_final,by="site_id")

###############################################################
###############################################################
# EXTRA YIELD INFO
library("readxl")

extra_info <- read_excel("DATASETS/Obs_Seedset_Insects_per_hour.xls") %>%
  rename(site_id=Landscape_ID,seeds_per_fruit=Nr_seeds,
         seed_weight=Weight_1000_seeds_gr) %>%
  select(site_id,seeds_per_fruit,seed_weight) %>%
  mutate(seed_weight=seed_weight/10) %>% group_by(site_id) %>%
  summarise_all(mean)

# sanity check

extra_info$site_id[!extra_info$site_id %in% data.site$site_id] %>% unique()

extra_info$site_id[extra_info$site_id=="2.42"] <- "2.4.2"
extra_info$site_id[extra_info$site_id=="4.01"] <- "4.1"
extra_info$site_id[extra_info$site_id=="2.01"] <- "2.0.1"
extra_info$site_id[extra_info$site_id=="4.12"] <- "4.1.2"
extra_info$site_id[extra_info$site_id=="5.01"] <- "5.0.1"
extra_info$site_id[extra_info$site_id=="2.02"] <- "2.0.2"

extra_info$site_id[!extra_info$site_id %in% data.site$site_id] %>% unique()

data.site <- data.site %>% left_join(extra_info,by="site_id")

###############################################################
###############################################################
###############################################################

field_level_data <- tibble(
  study_id=data.site$study_id,
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
  sampling_start_month=6,
  sampling_end_month=8,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
  plant_density=NA,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=NA,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  richness_restriction=NA,
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
  total_sampled_time=data.site$observationtime_min,
  visitation_rate_units="visits per 100 flowers and hour",
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
  Email_contact= "georg.andersson@cec.lu.se"
)
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Georg_Andersson_Brassica_rapa_Sweden_2010.csv")
setwd(dir_ini)
