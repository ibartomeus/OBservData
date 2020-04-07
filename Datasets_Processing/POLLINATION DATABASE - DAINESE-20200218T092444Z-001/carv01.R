
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Carv01: carv01
##########################


data.site <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="carv01")

data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site$Crop.species <- "Mangifera indica"

data.site <- data.site %>% select(-'Annual/perennial',-Management.2,-Management.3)%>%
  rename(study_id=StudyID,site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management,variety=cultivar)

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))  
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD

data.Functioning <- read.xlsx("DATASETS/Carv01_DataCollection_Pollination.xlsx",
                              sheet = "Functioning", startRow = 2)
data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(grepl("carv01",data.Functioning$SiteID,ignore.case = TRUE))

data.Functioning$Exclosure.treatment[data.Functioning$Exclosure.treatment=="pollinators not excluded"] <- "pollinators_not_excluded"
data.Functioning$Exclosure.treatment[data.Functioning$Exclosure.treatment=="ants excluded"] <- "ants_excluded"
data.Functioning$Exclosure.treatment[data.Functioning$Exclosure.treatment=="all visitors excluded"] <- "all_visitors_excluded"

data.Functioning <- data.Functioning %>% spread(key=Exclosure.treatment, value=Function)

data.Functioning <- data.Functioning %>% select(-Notes) %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,yield=pollinators_not_excluded,
         yield_units=Type.of.function,yield_treatments_no_pollinators=all_visitors_excluded,
         yield_treatments_pollen_supplement=ants_excluded)

data.site <- data.site %>% left_join(data.Functioning,by=c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.site$field_size <- NA


data.site <- data.site %>% mutate(country="South Africa",Publication="Carvalheiro et al. 2010. Journal of applied ecology",
                                  Credit="Luísa G. Carvalheiro",
                                  email="lgcarvalheiro@gmail.com")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

names(data.species)[12] <- "NOTES_1"
data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance_corrected= `Abundance.(corrected.for.differences.in.sampling.effort)`,
                                        Organism_ID=OrganismID,abundance_NO_corrected= `Abundance.(no.correction.for.sampling.effort)`)

data.species_01 <- data.species %>% filter(grepl("carv01",data.species$site_id,ignore.case = TRUE))
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

#data.species_01$Guild[data.species_01$Organism_ID=="Bombus spp. unknown or unidentified "] <- "bumblebees"
#data.species_01$Guild[data.species_01$Organism_ID=="Clytus arietus "] <- "beetles"

#data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="sweep net, note that flower abundance (hence sampling effort) varied between sites") 


insect_sampling <- tibble(
  study_id = "carv01",
  site_id = data.species_01$site_id,
  pollinator = data.species_01$Organism_ID,
  guild = data.species_01$Guild,
  sampling_method = data.species_01$sampling_method,
  abundance = data.species_01$abundance_NO_corrected,
  total_sampled_area = data.species_01$total_sampled_area,
  total_sampled_time = data.species_01$total_sampled_time,
  total_sampled_flowers = data.species_01$total_sampled_flowers,
  Description = data.species_01$Description
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_carv01.csv")

setwd(dir_ini)
                              

#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################
data.species_01$abundance_NO_corrected <- as.numeric(data.species_01$abundance_NO_corrected)

abundance_aux <- data.species_01 %>% 
  group_by(site_id,Guild) %>% count(wt=abundance_NO_corrected) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles","honeybees","lepidoptera","non_bee_hymenoptera",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>% 
  select(site_id,Organism_ID,abundance_NO_corrected)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance_NO_corrected)

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

x <- data.species_01 %>% group_by(site_id,Organism_ID,abundance_corrected)%>% count()
#The organisms are different at each site. Thus we can simply add guild corrected abundance in
# each site

visitation <- data.species_01 %>% group_by(site_id,Guild) %>% summarise(visits_flo=sum(abundance_corrected))

visitation <- visitation %>% spread(Guild, visits_flo)

names(visitation)


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
write_csv(field_level_data, "field_level_data_carv01.csv")
setwd(dir_ini)
