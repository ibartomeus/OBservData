
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()

##########################
#Data: DAINESE, Garr01: garr03
##########################


data.site <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="garr04")
options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

data.site$crop <- "Brassica napus"

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# VISITATION RATE
# Note that there are three treatments: Closed, supplementary, open

data.Functioning <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(StudyID=="garr04") %>% select(-StudyID)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

visitation_aux <- data.Functioning %>% filter(is.na(Exclosure.treatment))

#Chage visitation units
#[visits in 100 flowers during one hour] -> [visits in 100 flowers during one hour]

visitation_aux$Type.of.function <- 100*60*visitation_aux$Type.of.function

visitation_aux$Notes[is.na(visitation_aux$Notes)] <- "visitation_rate"
visitation_aux$Notes[visitation_aux$Notes=="bumblebee visits"] <- "visit_bombus"
visitation_aux$Notes[visitation_aux$Notes=="honeybee visits"] <- "visit_honeybee"
visitation_aux$Notes[visitation_aux$Notes=="hoverfly visits"] <- "visit_syrphids"
visitation_aux$Notes[visitation_aux$Notes=="solitary visits"] <- "visit_wildbees"

visitation_aux <- visitation_aux %>% select(-Exclosure.treatment,-Function) %>%
  spread(Notes,Type.of.function)

visitation_aux <- visitation_aux %>% mutate(visit_humbleflies=NA,
                                            visit_other_flies=NA,
                                            visit_beetles=NA,
                                            visit_lepidoptera=NA,
                                            visit_nonbee_hymenoptera=NA,
                                            visit_others=NA)

data.site <- data.site %>% left_join(visitation_aux, by = c("site_id","sampling_year"))

#######################################################################################
#######################################################################################
#Adding yield

yield_aux <- data.Functioning %>% filter(!is.na(Exclosure.treatment)) %>% select(-Notes)

# % pod set

yield_aux1 <- yield_aux %>% filter(Function=="% pod set") 

yield_aux1 <- yield_aux1 %>% spread(Exclosure.treatment,Type.of.function)

yield_aux1 <- yield_aux1 %>%
  rename(yield_treatments_no_pollinators=closed,
         yield=open,
         yield_units=Function)

data.site <- data.site %>% left_join(yield_aux1, by = c("site_id","sampling_year"))


# Seed per pod

yield_aux2 <- yield_aux %>% filter(Function=="Seed per pod") 

yield_aux2 <- yield_aux2 %>% spread(Exclosure.treatment,Type.of.function)

yield_aux2 <- yield_aux2 %>%
  rename(yield_treatments_no_pollinators2=closed,
         yield2=open,
         yield2_units=Function)

data.site <- data.site %>% left_join(yield_aux2, by = c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.LandscapeData <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                                 sheet = "LandscapeData", startRow = 2)
data.LandscapeData <- as_tibble(data.LandscapeData)
#data.LandscapeData$X <- as.numeric(data.LandscapeData$X)
#data.LandscapeData$Y <- as.numeric(data.LandscapeData$Y)
#data.LandscapeData$Field.size <- as.numeric(data.LandscapeData$Field.size)

data.LandscapeData <- data.LandscapeData %>% select(StudyID,SiteID,Field.size)%>%
  rename(study_id=StudyID,site_id=SiteID,field_size=Field.size)

data.LandscapeData <- unique(data.LandscapeData)

data.site <- data.site %>% left_join(data.LandscapeData, by = c("study_id","site_id"))
data.site <- data.site %>% mutate(country="UK",Publication="unpublished, Garratt et al. 2014a,b",
                                  Credit="Michael Garratt (University of Reading)",
                                  email="m.p.garratt@reading.ac.uk")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Garr01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=organismID)

data.species_04 <- data.species %>% filter(study_id=="garr04")
data.species_04 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_04 <- data.species_04 %>% select(-Identified.to,-X11)


data.species_04 <- data.species_04 %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data.species_04 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_04$Guild[data.species_04$Organism_ID=="Bombus spp. unknown or unidentified "] <- "bumblebees"


data.species_04 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_04 <- data.species_04 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="Measures per site: fruit set&size, seed set&size measured once on 10 plants in exclosures (1 open 1 closed, 10 treatment reps per site); pollinators sampled in 6 pan trap/transects in 3 sampling rounds; visitation rates in 6 2m² quadrats in 3 sampling rounds") 


insect_sampling <- tibble(
  study_id = "garr04",
  site_id = data.species_04$site_id,
  pollinator = data.species_04$Organism_ID,
  guild = data.species_04$Guild,
  sampling_method = data.species_04$sampling_method,
  abundance = data.species_04$abundance,
  total_sampled_area = data.species_04$total_sampled_area,
  total_sampled_time = data.species_04$total_sampled_time,
  total_sampled_flowers = data.species_04$total_sampled_flowers,
  Description = data.species_04$Description
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_garr04.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_04 %>% filter(sampling_method=="transect") %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles","bumblebees","honeybees","lepidoptera",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(non_bee_hymenoptera=0,other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_04 %>% filter(sampling_method=="transect") %>%
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
  study_id=data.site$study_id,
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
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
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
  visitation_rate_units = "visits per 100 flowers and hour",
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
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_garr04.csv")
setwd(dir_ini)

