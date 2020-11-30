
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()

##########################
#Data: DAINESE, Bore01: bore01
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Bore01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)


options(digits=14)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management) %>% mutate(study_id="Virginie_Boreux_Coffea_canephora_India_2008")

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,2:3], proj4string=CRS("+proj=utm +zone=32 +datum=WGS84"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#data.site$longitude <- NA
#data.site$latitude <- NA
#data.site[,10:11] <- spgeo@coords

#####################################
# YIELD
# Note that there are three treatments: hand,open,sealed

data.Functioning <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Bore01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

#yield_types <- c("initial fruitset"="yield0","final fruitset"="yield",
#                 "dry weight (mean per berry)"="yield2","%bean>6.65"="yield3")

#data.Functioning$yield_types <- unname(yield_types[data.Functioning$Type.of.function])

yield_aux1 <- data.Functioning %>% spread(Type.of.function,Function)

yield_aux1 <- yield_aux1 %>% mutate(yield_units="final fruitset (%)",
                                    yield2_units="initial fruitset (%)")

yield_aux1 <- yield_aux1 %>%
  rename(yield=`final fruitset`,
         yield2=`initial fruitset`,
         fruit_weight=`dry weight (mean per berry)`)

#TO percentage
yield_aux1$yield <- 100*yield_aux1$yield
yield_aux1$yield2 <- 100*yield_aux1$yield2

yield_aux1 <- select(yield_aux1,-`%bean>6.65`,-Exclosure.treatment)

data.site <- data.site %>% left_join(yield_aux1, by = c("site_id","sampling_year"))


###########################
# Adding  Field_size

data.site$field_size <- NA
data.site <- data.site %>% mutate(country="India",Publication="10.1007/s13593-016-0377-7,10.1016/j.agee.2012.05.003,10.1073/pnas.1210590110",
                                  Credit="Virginie Boreux (now at Freiburg Uni), ETH Zurich",
                                  email="virginie.boreux@nature.uni-freiburg.de")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Bore01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% mutate(Abundance=round(`Abundance.(mean.abundance.for.15min.obs.and.30.inflorescences)`*Number.of.censuses))
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)
data.species <- data.species %>% mutate(total_time=10*15*Number.of.censuses)

data.species_04 <- data.species

# Evaluate the percentage of species + morphospecies
data.species_04 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <- sum(data.species_04$Identified.to %in% c("species"))/nrow(data.species_04)

data.species_04 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_04 <- data.species_04 %>% select(-Identified.to,-X6)


data.species_04 <- data.species_04 %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data.species_04 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX
data.species_04 <- data.species_04 %>% filter(!is.na(Organism_ID))
data.species_04 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_04 <- data.species_04 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=15*Number.of.censuses,
                                              total_sampled_flowers=NA,
                                              Description="for insect observation: 3 to 8x15min observations, encompassing 5 branches of 6 clusters each. ")


insect_sampling <- tibble(
  study_id = "Virginie_Boreux_Coffea_canephora_India_2008",
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

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Virginie_Boreux_Coffea_canephora_India_2008.csv")

# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

data.species_04 %>% group_by(sampling_method) %>% count()

abundance_aux <- data.species_04 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles","bumblebees","honeybees","lepidoptera","non_bee_hymenoptera","other",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0, syrphids=0, humbleflies=0,
                                          other_flies=0, beetles=0, non_bee_hymenoptera=0,
                                          lepidoptera=0, other=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_04 %>% filter(abundance>0)%>%
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

time <- data.species %>% group_by(site_id,total_time) %>% count() %>% select(-n)
data.site <- data.site %>% left_join(time, by = "site_id")
###############################################################
###############################################################
# VISITATION RATE

number_census_site <- data.species_04 %>% filter(abundance>0)%>%
  group_by(site_id,Number.of.censuses) %>% count()

number_census_site <- mutate(number_census_site,time_obs=10*15*Number.of.censuses,
                             branc_infl=10*6*5*Number.of.censuses) %>%
  select(-n)

visit_aux <- number_census_site %>% left_join(abundance_aux, by="site_id")

for (i in 1:nrow(visit_aux)){
  adap_unit <- 60/pull(visit_aux[i,3]*visit_aux[i,4])
  visit_aux[i,5:ncol(visit_aux)] <- adap_unit*visit_aux[i,5:ncol(visit_aux)]

}

visit_aux <- visit_aux %>% rename(

  visitation_rate= total,
  visit_honeybee= honeybees,
  visit_bombus= bumblebees,
  visit_wildbees= other_wild_bees,
  visit_syrphids= syrphids,
  visit_humbleflies= humbleflies,
  visit_other_flies= other_flies,
  visit_beetles= beetles,
  visit_lepidoptera= lepidoptera,
  visit_nonbee_hymenoptera= non_bee_hymenoptera,
  visit_others= other,
)
data.site <- data.site %>% left_join(visit_aux, by = "site_id")

data.site$total_time==data.site$time_obs

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
  fruit_weight=data.site$fruit_weight,
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
  total_sampled_area=NA,
  total_sampled_time=data.site$total_time,
  visitation_rate_units="visits per inflorescence and hour",
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
# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Virginie_Boreux_Coffea_canephora_India_2008.csv")
# setwd(dir_ini)
