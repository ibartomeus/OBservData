
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Carv01: carv02
##########################


data.site <- read.xlsx("DATASETS/Carv01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(StudyID=="carv02")

data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site$Crop.species <- "Helianthus annuus"

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

data.Functioning <- data.Functioning %>% filter(grepl("carv02",data.Functioning$SiteID,ignore.case = TRUE))

data.Functioning$Exclosure.treatment[data.Functioning$Exclosure.treatment=="pollinators not excluded"] <- "pollinators_not_excluded"
data.Functioning$Exclosure.treatment[data.Functioning$Exclosure.treatment=="pollinators excluded"] <- "all_visitors_excluded"

data.Functioning <- data.Functioning %>% spread(key=Exclosure.treatment, value=Function)

data.Functioning <- data.Functioning %>% select(-Notes) %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,yield=pollinators_not_excluded,
         yield_units=Type.of.function,yield_treatments_no_pollinators=all_visitors_excluded)

data.site <- data.site %>% left_join(data.Functioning,by=c("site_id","sampling_year"))


#More yield 
data.Final <- read.xlsx("DATASETS/Carv01_DataCollection_Pollination.xlsx",
                        sheet = "Final Ecosystem Services", startRow = 2)
data.Final <- as_tibble(data.Final)

data.Final <- data.Final %>% filter(grepl("carv02",data.Final$SiteID,ignore.case = TRUE))
#El apartado de yield repite lo mismo que fruit set para no exclosures

###########################
# Adding  Field_size

data.site$field_size <- NA


data.site <- data.site %>% mutate(country="South Africa",Publication="Carvalheiro et al. 2011. Ecology Letters",
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

data.species_01 <- data.species %>% filter(grepl("carv02",data.species$site_id,ignore.case = TRUE))
data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
  
data.species_01 <- data.species_01 %>% select(-Identified.to)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01$Guild[data.species_01$Organism_ID=="Junonia oenone "] <- "lepidoptera"
#data.species_01$Guild[data.species_01$Organism_ID=="Clytus arietus "] <- "beetles"

data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=24,
                                              total_sampled_flowers=NA,
                                              Description="Total numbe of visitors per 90 sunflower heads. Note that flower abundance (hence sampling effort) varied between sites. 6 (3 x 4min censuses, repeated in the morning and afternoon. Total observation time per plant = 24min)") 


insect_sampling <- tibble(
  study_id = "carv02",
  site_id = data.species_01$site_id,
  pollinator = data.species_01$Organism_ID,
  guild = data.species_01$Guild,
  sampling_method = data.species_01$sampling_method,
  abundance = data.species_01$abundance_corrected,
  total_sampled_area = data.species_01$total_sampled_area,
  total_sampled_time = data.species_01$total_sampled_time,
  total_sampled_flowers = data.species_01$total_sampled_flowers,
  Description = data.species_01$Description
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_carv02.csv")

setwd(dir_ini)
                              

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>% 
  select(site_id,Organism_ID,abundance_corrected)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance_corrected)

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
  mutate(richness_estimator_method="Observed")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")
###############################################################
###############################################################

x <- data.species_01 %>% group_by(site_id,Organism_ID,abundance_corrected)%>% count()
#The organisms are different at each site. Thus we can simply add guild corrected abundance in
# each site

visitation <- data.species_01 %>% group_by(site_id,Guild) %>% summarise(visits_flo=sum(abundance_corrected))

visitation <- visitation %>% spread(Guild, visits_flo)

names(visitation)

#We need average number of flower heads/plant to estimate visitation rates in visits/heads/min

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
write_csv(field_level_data, "field_level_data_carv02.csv")
setwd(dir_ini)
