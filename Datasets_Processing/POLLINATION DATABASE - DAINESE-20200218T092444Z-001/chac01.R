
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Chac01: chac01
##########################


data.site <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(SiteID=="chac01")

data.site <- data.site %>%
  separate(col = Distance.ID, into = c("site", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

data.site <- data.site %>%
  separate(col = Crop.species, into = c("Crop.species", "variety"),". Cultivar: ")


data.site <- data.site %>% filter(is.na(dis)) %>% select(-dis)

data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial',-X10,-X11) %>%
  rename(study_id=SiteID,site_id= site, X_UTM=X,Y_UTM=Y,zone_UTM=`X,Y.Projection.&.Datum`,
         sampling_year=Year,crop=Crop.species,
         management=Management)

data.site$zone_UTM <- 20

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=20 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

data.site$longitude <- NA
data.site$latitude <- NA
data.site[,10:11] <- spgeo@coords

#####################################
# YIELD
# Note that there are 2 treatments: supplementary, open

data.Functioning <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(SiteID=="chac01",Year.of.sampling==2000)

data.Functioning <- data.Functioning %>%
  separate(col = Distance.ID, into = c("site", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

data.Functioning$site[data.Functioning$site=="penia"] <- "penia colorada"

data.Functioning <- data.Functioning %>% select(-dis,Pollen.grains.ALLOGAMY,
                                                -Pollen.grains.OPEN.FLOWERS,
                                                -Pollen.grains.ALLOGAMY,
                                                -Fruit.set.for.EXCLOSURES,-Function)

data.Functioning <- data.Functioning %>% group_by(SiteID,site,Year.of.sampling) %>%
  summarise(yield_treatments_pollen_supplement=100*mean(Fruit.set.ALLOGAMY),
            yield=100*mean(fruit.set.OPEN.FLOWERS),
            yield_units="fruit set (%)") %>%
  rename(study_id=SiteID,site_id=site,sampling_year=Year.of.sampling)


data.site <- data.site %>% left_join(data.Functioning, by = c("study_id","site_id","sampling_year"))


###########################
# Adding  Field_size

data.site$field_size <- NA

data.site <- data.site %>% mutate(country="Argentina",Publication="10.1111/j.1365-2664.2005.01116.x,10.1098/rspb.2007.1547",
                                  Credit="Natacha Chacoff (Instituto de Ecologia Regional. CONICET UNT)",
                                  email="nchacoff@gmail.com")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>% filter(SiteID=="chac01",Year.of.sampling==2000)

# Be careful !! Abundance here reflects: no. visits × 15 min???1 × flower???1
# No information about flower has been provided



#We are going to estimate the number of census

data.meta <- read.xlsx("DATASETS/Chac01_Datacollection_pollination.xlsx",
                          sheet = "StudyMetadata", startRow = 2)

data.meta <- as_tibble(data.meta)

data.meta <- data.meta %>% filter(StudyID=="chac01",`Study.year(s)`==2000)

data.meta <- data.meta %>% select(Distance.ID,Number.of.visits.census)

data.species <- data.species %>% left_join(data.meta,by="Distance.ID")

data.species <- data.species %>% select(-Number.of.censuses)

data.species$Sampling.method <- paste(data.species$Number.of.visits.census,"census of 15 minutes observation to a flowering branch",sep=" ")


data.species <- data.species %>% rename(site_id=Distance.ID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to,-X7)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Bombus spp. unknown or unidentified "] <- "bumblebees"
data.species_01$Guild[data.species_01$Organism_ID=="Clytus arietus "] <- "beetles"

data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=Number.of.visits.census*15,
                                              total_sampled_flowers=NA,
                                              Description="Abundance here reflects: no. visits per 15 min and flower") 


insect_sampling <- tibble(
  study_id = "chac01",
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
write_csv(insect_sampling, "insect_sampling_chac01.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

# NO DATA For ABUNDANCE

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

#We relabel sites to aggregate different distances

data.species_aux <- data.species_01 %>%
  separate(col = site_id, into = c("site_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

data.species_aux$site_id[data.species_aux$site_id=="penia"] <- "penia colorada"

abundace_field <- data.species_aux %>%
  select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  #chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  #abundace_field$r_obser[i] <-  chao$Observed
  #abundace_field$r_chao[i] <-  chao$Estimator 
  abundace_field$r_obser[i] <-  sum(x>0)
  abundace_field$r_chao[i] <-  sum(x>0)
}

richness_aux <- abundace_field %>% select(site_id, r_chao)
richness_aux <- richness_aux %>% rename(pollinator_richness=r_chao) %>%
  mutate(richness_estimator_method="observed")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


######################################################
# VISITATION RATE
######################################################

#We relabel sites to aggregate different distances

data.species_aux <- data.species_01 %>%
  separate(col = site_id, into = c("site_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

data.species_aux$site_id[data.species_aux$site_id=="penia"] <- "penia colorada"

visitation_aux <- data.species_aux %>%
  select(site_id,Organism_ID,abundance,Guild)%>%
  group_by(site_id,Organism_ID,Guild) %>% summarise(mean_visit=mean(abundance))

#Addapting units visits/100flowers/h

visitation_aux$mean_visit <- visitation_aux$mean_visit/25

#The organisms at different sites are different. Thus, we add their corresponding visitation rates 

visitation_aux <- visitation_aux %>%
  group_by(site_id,Guild) %>% summarise(mean_visit=sum(mean_visit))


visitation_aux <- visitation_aux %>% spread(Guild, mean_visit)

names(visitation_aux)

#"beetles"             "honeybees"          
#[5] "lepidoptera"         "non_bee_hymenoptera" "other_flies"         "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visitation_aux <- mutate(visitation_aux,bumblebees=0,syrphids=0, humbleflies=0,other=0,total=0)

visitation_aux[is.na(visitation_aux)] <- 0
visitation_aux$total <- rowSums(visitation_aux[,c(3:ncol(visitation_aux))],na.rm = T)

visitation_aux[is.na(visitation_aux)] <- 0

data.site <- data.site %>% left_join(visitation_aux, by = "site_id")

#Average observation time per site
av_time <- data.species_aux %>% group_by(site_id) %>% summarise(mean_time=15*mean(Number.of.visits.census))
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
  X_UTM=data.site$X_UTM,
  Y_UTM=data.site$Y_UTM,
  zone_UTM=data.site$zone_UTM,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
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
  abundance=NA,
  ab_honeybee=NA,
  ab_bombus=NA,
  ab_wildbees=NA,
  ab_syrphids=NA,
  ab_humbleflies=NA,
  ab_other_flies=NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others=NA,
  total_sampled_area=NA,
  total_sampled_time=data.site$mean_time,
  visitation_rate_units="(average number of) visits per 100 flowers and hour",
  visitation_rate=data.site$total,
  visit_honeybee=data.site$honeybees,
  visit_bombus=data.site$bumblebees,
  visit_wildbees=data.site$other_wild_bees,
  visit_syrphids=data.site$syrphids,
  visit_humbleflies=data.site$humbleflies,
  visit_other_flies=data.site$other_flies,
  visit_beetles=data.site$beetles,
  visit_lepidoptera=data.site$lepidoptera,
  visit_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  visit_others=data.site$other,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_chac01.csv")
setwd(dir_ini)

