
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, krish01: krish013
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Krish01_DataCollection_Pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>% filter(Year==2009)
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,latitude=X,longitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management) %>% mutate(study_id="krish013")

data.site$management <- unname(management_types[data.site$management])

# Each field is separated into multiple location. Hereafter we use
# aggregate measures and locate the fields at "0

data.site <- data.site %>%
  separate(col = site_id, into = c("site_id", "distance"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

data.site <- data.site %>% filter(distance==0) %>% select(-distance)


##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))


#####################
# SAMPLING MONTHS

sampling <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/flowering_dates_sitewise_2007_2008_2009_2014.xlsx")

# Fix sampling start month

sampling %>% group_by(sampling_start_month) %>% count()

sampling$sampling_start_month[sampling$sampling_start_month=="april"] <- 4
sampling$sampling_start_month[sampling$sampling_start_month=="february"] <- 2
sampling$sampling_start_month[sampling$sampling_start_month=="january"] <- 1
sampling$sampling_start_month[sampling$sampling_start_month=="march"] <- 3

sampling %>% group_by(sampling_start_month) %>% count()

# Fix sampling end month

sampling %>% group_by(sampling_end_month) %>% count()

sampling$sampling_end_month[sampling$sampling_end_month=="april"] <- 4
sampling$sampling_end_month[sampling$sampling_end_month=="february"] <- 2
sampling$sampling_end_month[sampling$sampling_end_month=="january"] <- 1
sampling$sampling_end_month[sampling$sampling_end_month=="march"] <- 3
sampling$sampling_end_month[sampling$sampling_end_month=="may"] <- 5

sampling %>% group_by(sampling_end_month) %>% count()

# Sampling for a given year

sampling_year <- sampling %>% filter(sampling_year==2009) %>%
  select(-study_id,-sampling_year)

# Sanity check
all(sampling_year$site_id %in% data.site$site_id)

data.site <-  data.site %>% left_join(sampling_year,by="site_id")

data.site$sampling_start_month <-  as.numeric(data.site$sampling_start_month)
data.site$sampling_end_month <-  as.numeric(data.site$sampling_end_month)

#####################################
# YIELD
# Note that there is only treatment: open

data.Functioning <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Krish01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>% filter(Year.of.sampling==2009)

data.Functioning <- data.Functioning %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling)

#Adding yield

yield_aux <- data.Functioning %>% filter(!is.na(Comments))

# Final seedset

yield_aux1 <- yield_aux %>% filter(Type.of.function=="final seedset")

yield_aux1 <- yield_aux1 %>% spread(Exclosure.treatment,Type.of.function)

# adjusting yield units
yield_aux1$Function <- 100*yield_aux1$Function

yield_aux1$Comments <- "final seedset: percentage of ovules that developed into seeds at harvest"

yield_aux1 <- yield_aux1 %>%
  rename(yield=Function,
         yield_units=Comments) %>% select(-`open pollination`)

yield_aux1 <- yield_aux1 %>%
  separate(col = site_id, into = c("site_id", "distance"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

yield_aux11 <- yield_aux1 %>% group_by(site_id) %>% summarise(sampling_year=mean(sampling_year),
                                               yield=mean(yield,na.rm = TRUE)) %>%
  mutate(yield_units=NA)

yield_aux11$yield_units <- yield_aux1$yield_units[1]

data.site <- data.site %>% left_join(yield_aux11, by = c("site_id","sampling_year"))

# final fruitset

yield_aux2 <- yield_aux %>% filter(Type.of.function=="final fruitset")

yield_aux2 <- yield_aux2 %>% spread(Exclosure.treatment,Type.of.function)

# adjusting yield units
yield_aux2$Function <- 100*yield_aux2$Function

yield_aux2$Comments <- "final fruitset: percentage of flowers that developed into mature fruits at harvest"

yield_aux2 <- yield_aux2 %>%
  rename(yield2=Function,
         yield2_units=Comments) %>% select(-`open pollination`)

yield_aux2 <- yield_aux2 %>%
  separate(col = site_id, into = c("site_id", "distance"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")

yield_aux21 <- yield_aux2 %>% group_by(site_id) %>% summarise(sampling_year=mean(sampling_year),
                                                              yield2=mean(yield2,na.rm = T)) %>%
  mutate(yield2_units=NA)

yield_aux21$yield2_units <- yield_aux2$yield2_units[1]

data.site <- data.site %>% left_join(yield_aux21, by = c("site_id","sampling_year"))

###########################
# Adding  Field_size

data.site$field_size <- NA

data.site <- data.site %>% mutate(country="India",Publication="10.1016/j.baae.2012.03.007",
                                  Credit="Smitha Krishnan, ETH Zurich",
                                  email="smithakrishnan@gmail.com / S.Krishnan@cgiar.org")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Krish01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(sampling_year==2009)

# Evaluate the percentage of species + morphospecies
data.species_01 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_01$Identified.to %in% c("Species"))/nrow(data.species_01[!is.na(data.species_01$Identified.to),])

data.species_01 %>% group_by(sampling_method) %>% count()


gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_01 <- data.species_01 %>% select(-Identified.to,-X6)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data.species_01 <- data.species_01 %>% filter(Organism_ID!="0")
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

#data.species_01$Guild[data.species_01$Organism_ID=="Muscidae (like a house fly)"] <- "other_flies"
#data.species_01$Guild[data.species_01$Organism_ID=="Stick insect"] <- "other"


data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()


#reported abundances are the average of three observations
data.species_01$abundance <- round(5*data.species_01$abundance)


data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=75,
                                              total_sampled_flowers=NA,
                                              Description="Locations with 5 distance classes (<10, 50, 100, 250, 500m) measured from the edge of a forest fragment within each location. Few had to be discarded due to rains on day of observation. Measures per site: 5 branches per plant with 6 inflorescence per branch. 5 plants. Observations last 15 mins each. 1 temporal replicate")



insect_sampling <- tibble(
  study_id = "Smitha_Krishnan_Coffea_canephora_India_2009",
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

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Smitha_Krishnan_Coffea_canephora_India_2009.csv")

# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

#Aggration of locations

data.species_01 <- data.species_01 %>%
  separate(col = site_id, into = c("site_id", "distance"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])")


abundance_aux <- data.species_01 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are beetles, "honeybees","","non_bee_hymenoptera",other,
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,bumblebees=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

# fixing site names
abundance_aux$site_id <- paste(abundance_aux$site_id,"new",sep="")


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
  #abundace_field$r_obser[i] <-  sum(x>0)
  #abundace_field$r_chao[i] <-  sum(x>0)
}


richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos<0.75){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

# fixing site names
richness_aux$site_id <- paste(richness_aux$site_id,"new",sep="")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


####################
# Number of sites in each farm

number_sites <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Krish01_DataCollection_Pollination.xlsx",
                          sheet = "SiteData", startRow = 2)

number_sites <- as_tibble(number_sites)

number_sites <- number_sites %>% filter(Year==2009)

number_sites <- number_sites %>%
  separate(col = SiteID, into = c("location", "distance"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])") %>%
  group_by(location) %>% count() %>% rename(site_id=location)


###############################################################
###############################################################
#VISITATION RATE

visit_aux <- abundance_aux

visit_aux <- number_sites %>% left_join(visit_aux, by ="site_id")


#total time observation per site = number of sites *5*15=75*n
#total number of inflorescences = 5*n*5*6=150*n

for (i in 1:nrow(visit_aux)){
  a <- 75*150*pull(visit_aux[i,2])*pull(visit_aux[i,2])
  visit_aux[i,3:ncol(visit_aux)] <- 60*visit_aux[i,3:ncol(visit_aux)]/a

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
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="Smitha_Krishnan_Coffea_canephora_India_2009",
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety="Robusta",
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=data.site$sampling_start_month,
  sampling_end_month=data.site$sampling_end_month,
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
  fruit_weight=NA,
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
  total_sampled_time=75*data.site$n, #75min de observaci?n en cada punto de observaci?n de cada granja
  visitation_rate_units="(average number of) visits per inflorescence and hour",
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
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Smitha_Krishnan_Coffea_canephora_India_2009.csv")
# setwd(dir_ini)

