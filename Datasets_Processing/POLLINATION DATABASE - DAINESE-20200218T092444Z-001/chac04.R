
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Chac01: chac04
##########################


data.site <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(SiteID=="chac04")

data.site <- data.site %>%
  separate(col = Crop.species, into = c("Crop.species", "variety"),". Cultivar ")

data.site$X <- -1*as.numeric(data.site$X)
data.site$Y <- -1*as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial',-X10,-X11) %>%
  rename(study_id=SiteID,site_id= Distance.ID, longitude=Y,latitude=X,zone_UTM=`X,Y.Projection.&.Datum`,
         sampling_year=Year,crop=Crop.species,
         management=Management)

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=20 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#data.site$longitude <- NA
#data.site$latitude <- NA
#data.site[,10:11] <- spgeo@coords

#####################################
# YIELD
# Note that there are 2 treatments: supplementary, open

data.Functioning <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(Year.of.sampling==2015)

data.Functioning <- data.Functioning %>% select(-SiteID,-Fruit.set.ALLOGAMY,-Pollen.grains.ALLOGAMY,
                                                -Pollen.grains.OPEN.FLOWERS,
                                                -Pollen.grains.ALLOGAMY,
                                                -Function)

data.Functioning <- data.Functioning %>%
  mutate(yield_treatments_no_pollinators=100*Fruit.set.for.EXCLOSURES,
            yield=100*fruit.set.OPEN.FLOWERS,
            yield_units="fruit set (%)") %>%
  rename(site_id=Distance.ID,sampling_year=Year.of.sampling) %>%
  select(-Fruit.set.for.EXCLOSURES,-fruit.set.OPEN.FLOWERS)


data.site <- data.site %>% left_join(data.Functioning, by = c("site_id","sampling_year"))


###########################
# Adding  Field_size

data.site$field_size <- NA

data.site <- data.site %>% mutate(country="Argentina",Publication=NA,
                                  Credit="Natacha Chacoff (Instituto de Ecologia Regional. CONICET UNT)",
                                  email="nchacoff@gmail.com")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(Year.of.sampling==2015)

# Be careful !! Abundance is given in counts, not no. visits × 15 min???1 × flower!!!
# No information about flower has been provided



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
                                        Organism_ID=OrganismID)

data.species_01 <- data.species
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("../Tesauro_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to,-X7)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Avispa negra "] <- "non_bee_hymenoptera"
data.species_01$Guild[data.species_01$Organism_ID=="Avispa rayada "] <- "non_bee_hymenoptera"
data.species_01$Guild[data.species_01$Organism_ID=="Bombus negro "] <- "bumblebees"
data.species_01$Guild[data.species_01$Organism_ID=="Ligaidae"] <- "other"
data.species_01$Guild[data.species_01$Organism_ID=="Ligaidae "] <- "other"
data.species_01$Guild[data.species_01$Organism_ID=="Mosca brillante  "] <- "other_flies"

data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=Number.of.visits.census*15,
                                              total_sampled_flowers=NA,
                                              Description=NA) 


insect_sampling <- tibble(
  study_id = "chac04",
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
write_csv(insect_sampling, "insect_sampling_chac04.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_01 %>% 
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles","bumblebees","honeybees","lepidoptera","non_bee_hymenoptera","other",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(humbleflies=0,total=0)
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
#Average observation time per site
av_time <- data.species %>% group_by(site_id) %>% summarise(mean_time=15*mean(Number.of.visits.census))
data.site <- data.site %>% left_join(av_time, by = "site_id")

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
  total_sampled_time=data.site$mean_time,
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
write_csv(field_level_data, "field_level_data_chac04.csv")
setwd(dir_ini)
