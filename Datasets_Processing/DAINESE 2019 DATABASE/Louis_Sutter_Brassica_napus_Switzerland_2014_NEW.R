
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Sutt01: sutt01
##########################


data.site <- read.xlsx("DATASETS/Sutt01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(StudyID,SiteID,Year,Crop_species,X,Y,Management)%>%
  rename(study_id=StudyID,site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop_species,
         management=Management)
 
# Fix crop name

data.site$crop <- "Brassica napus"

data.site <- data.site %>% separate(site_id,c("site_id","sub"),"-") %>% select(-sub)
data.site <- unique(data.site)

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD

data.Functioning <- read.xlsx("DATASETS/Sutt01_Datacollection_pollination.xlsx",
                                 sheet = "CropFunctionData", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

yield_aux1 <- data.Functioning %>% 
  filter(Function_data_type =="yield")

yield_aux1 <- select(yield_aux1,-StudyID,-StudyID_yr,-Exclosure_treatment,-Block,-Function_data_type)

#fixing units

yield_aux1$Function_data <- 1000*yield_aux1$Function_data
yield_aux1$Unit <- "kg/ha"

yield_aux1 <- yield_aux1 %>%
  rename(site_id=SiteID,sampling_year=Year,
         yield=Function_data,
         yield_units=Unit)

yield_aux1 <- yield_aux1 %>% separate(site_id,c("site_id","sub"),"-") %>% select(-sub)
yield_aux1 <- yield_aux1 %>% group_by(site_id,sampling_year,yield_units) %>%
  summarise(yield=mean(yield))


data.site <- data.site %>% left_join(yield_aux1, by = c("site_id","sampling_year"))


####################
#SEED SET

yield_aux2 <- data.Functioning %>% 
  filter(Function_data_type =="seedset")

yield_aux2 <- select(yield_aux2,-StudyID,-StudyID_yr,-Block,-Function_data_type)

#fixing units

yield_aux2$Unit <- "seedset (number of seeds per pod)"

yield_aux2 <- yield_aux2 %>% spread(Exclosure_treatment,Function_data)

yield_aux2 <- yield_aux2 %>%
  rename(site_id = SiteID,sampling_year=Year,
         yield2 = open,
         yield2_units = Unit,
         yield_treatments_no_pollinators2 = closed,
         yield_treatments_pollen_supplement2 = `hand pollination`)

yield_aux2 <- yield_aux2 %>% separate(site_id,c("site_id","sub"),"-") %>% select(-sub)
yield_aux2 <- yield_aux2 %>% group_by(site_id,sampling_year,yield2_units) %>%
  summarise(yield2=mean(yield2),
            yield_treatments_no_pollinators2=mean(yield_treatments_no_pollinators2),
            yield_treatments_pollen_supplement2=mean(yield_treatments_pollen_supplement2))


data.site <- data.site %>% left_join(yield_aux2, by = c("site_id","sampling_year"))


###########################
# Adding  Field_size

data.site$field_size <- NA

data.site <- data.site %>% mutate(country="Switzerland",Publication=NA,
                                  Credit="Louis Sutter (Agroscope, Zurich, Switzerland), Project QuESSA",
                                  email="louis.sutter@agroscope.admin.ch")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Sutt01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% separate(SiteID,c("SiteID","sub"),"-") %>% select(-sub)

data.species <- data.species %>% rename(study_id=StudyID,site_id=SiteID,sampling_year=Year,
                                        sampling_method=Sampling_method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species

# Evaluate the percentage of species + morphospecies
data.species %>% filter(sampling_method %in% c("sweep")) %>%
  group_by(Identified_to) %>% count()
percentage_species_morphos <- 377/(67+3+141+377)

data.species %>% filter(sampling_method %in% c("sweep","pitfall")) %>%
  group_by(Identified_to) %>% count()
percentage_species_morphos_all <- 1096/(1096+208+266)


data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified_to)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family.y) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Asaphdion.flavipes"] <- "beetles"


data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family.y) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time = 4*Nb_censuses* Abundance_duration*24*60,
                                              total_sampled_flowers=NA,
                                              Description="4x(2 observations, 2 sweep, and 4 pitfalls) per site") 

data.species_01 <- data.species_01 %>%  filter(abundance!=0)

data.species_01 %>% group_by(Nb_censuses,sampling_method) %>% count()


insect_sampling <- tibble(
  study_id = "Louis_Sutter_Brassica_napus_Switzerland_2014",
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
write_csv(insect_sampling, "insect_sampling_Louis_Sutter_Brassica_napus_Switzerland_2014.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

#abundance with sweept net

abundance_aux <- data.species_01 %>% filter(sampling_method=="sweep") %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# "beetles"             "bumblebees"         
# [4] "honeybees"           "non_bee_hymenoptera" "other"              
# [7] "other_wild_bees"     "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(total=0,humbleflies=0,
                                          other_flies=0,lepidoptera=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>% filter(sampling_method=="sweep") %>%
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


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

#abundance with sweept net

visit_aux <- data.species_01 %>% filter(sampling_method=="obs") %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(visit_aux)

# "bumblebees"      "honeybees"       "humbleflies"    
# [5] "lepidoptera"     "other_flies"     "other_wild_bees" "syrphids"   

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(total=0,beetles=0,other=0,
                                          non_bee_hymenoptera=0,
                                          other_flies=0,lepidoptera=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(2:ncol(visit_aux))])

insect_sampling %>% filter(sampling_method=="obs") %>% 
  select(site_id,total_sampled_time) %>% group_by(site_id)%>%
  summarise_all(mean)

visit_rate <- bind_cols(visit_aux[,1],
                        (60/288)*visit_aux[,2:ncol(visit_aux)]) %>%
  rename(
    visitation_rate=total,
    visit_honeybee=honeybees,
    visit_bombus=bumblebees,
    visit_wildbees=other_wild_bees,
    visit_syrphids=syrphids,
    visit_humbleflies=humbleflies,
    visit_other_flies=other_flies,
    visit_beetles=beetles,
    visit_lepidoptera=lepidoptera,
    visit_nonbee_hymenoptera=non_bee_hymenoptera,
    visit_others=other
  )


data.site <- data.site %>% left_join(visit_rate, by = "site_id")


###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="Louis_Sutter_Brassica_napus_Switzerland_2014",
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
  total_sampled_time=4*2*0.025*24*60,
  visitation_rate_units = "visits per hour",
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
  Credit="Louis Sutter, Philippe Jeanneret, Matthias Albrecht (Agroscope, Zurich, Switzerland), Project QuESSA",
  Email_contact=data.site$email
)

# Louis update

update <- read.xlsx("DATASETS/Louis Sutter queries.xlsx")

# Update publication
field_level_data$Publication <- "10.1111/1365-2664.12977"

field_level_data$variety <- update$variety
field_level_data$field_size <- update$field_size
field_level_data$sampling_start_month <- update$sampling_start_month
field_level_data$sampling_end_month <- update$sampling_end_month
field_level_data$longitude <- update$latitude #Louis long/Lat are interch.
field_level_data$latitude <- update$longitude #Louis long/Lat are interch.


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Louis_Sutter_Brassica_napus_Switzerland_2014.csv")
setwd(dir_ini)
