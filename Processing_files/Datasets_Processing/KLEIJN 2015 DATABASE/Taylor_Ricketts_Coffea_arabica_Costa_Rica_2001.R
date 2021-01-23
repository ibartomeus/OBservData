
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 87_TaylorRickettsCoffee_2001
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/87_88_TaylorRickettsCoffee_2001_2002/Ricketts_beeabunds_Kleijn.xls",
                       sheet = "Sheet10")
data_raw <- as_tibble(data_raw)

# Data for 2001

data_raw <- data_raw %>% gather("site_id","abundance",c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,
                                            `12`,`21`,`22`,`23`,`24`,`101`,
                                            `102`,`103`,`104`,`105`,`106`,`107`,`108`,
                                            `109`,`110`,`111`,`112`))

data_raw$site_id <- as.numeric(data_raw$site_id)

#Select data for 2001

data_raw <- data_raw %>% filter(site_id <13,abundance>0)

##############
# Data site
##############

data.site <- data_raw %>% select(site_id) %>% unique()


# We add data site ID

data.site$study_id <- "Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001"
data.site$crop <- "Coffea arabica"
data.site$variety <- "Caturra"
data.site$management <- NA
data.site$country <- "Costa Rica"
data.site$latitude <- NA
data.site$longitude <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_year <- 2001
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
data.site$Publication <- "10.1073/pnas.0405147101"
data.site$Credit <- "Taylor H. Ricketts, Gretchen C. Daily, Paul R. Ehrlich, and Charles D. Michener"
data.site$Email_contact <- "taylor.ricketts@UVM.EDU"
data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA


###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>% rename(Organism_ID=species)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))



#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

# Native-summ, on the other hand, is a lumping of 13 morphospecies, each seen only a total of
# once or twice during the 2 year study.  I think you could safely exclude those.

list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"

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
  study_id = "Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "samples of flower visitors",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA, #we dont know the total amount of sampling days
  Description = "At each site, on each day in which coffee was in flower, 2 simultaneous samples of flower visitors were taken. Each sample involved recording each visitor and the number of flowers visited for 10 minutes on an area of one bush comprising approximately 250 flowers."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
#setwd(dir_ini)

#######################################
# VISITATION RATE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


visit_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)



names(visit_aux)

# There are "bumblebees" "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,
                                          bumblebees=0,total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(2:ncol(visit_aux))])

data.site <- data.site %>% left_join(visit_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
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

# Load our estimation for taxonomic resolution
percentage_species_morphos <- 0.9

#Since data refers to flower visits we can not estimate Chao1 index

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method=NA,richness_restriction="only bees")

richness_aux$other_pollinator_richness <- NA

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
  abundance = NA,
  ab_honeybee = NA,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = NA,
  visitation_rate_units = "flowers visited per hour",
  visitation_rate = 6*data.site$total,
  visit_honeybee = 6*data.site$honeybees,
  visit_bombus = 6*data.site$bumblebees,
  visit_wildbees = 6*data.site$other_wild_bees,
  visit_syrphids = 6*data.site$syrphids,
  visit_humbleflies = 6*data.site$humbleflies,
  visit_other_flies = 6*data.site$other_flies,
  visit_beetles = 6*data.site$beetles,
  visit_lepidoptera = 6*data.site$lepidoptera,
  visit_nonbee_hymenoptera = 6*data.site$non_bee_hymenoptera,
  visit_others = 6*data.site$other,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
#setwd(dir_ini)

# To calculate the visitation rate (flowers vitited per unit of time) of each guild,
# we aggregated the visitation rates of its corresponding organisms. Is it fine?

# We have no information on the number of days in which coffee was in flower. Thus, it was
# not possible to estimate total sampling time, etc.

# Observed richness was calculated from visitation rates.


#############################
# UPDATE
#############################

##########################
#Data: 87_TaylorRickettsCoffee_2001
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/87_88_TaylorRickettsCoffee_2001_2002/field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001_UPDATED.xlsx",
                       sheet = "field_level_data_Taylor_Rickett")
data_raw <- as_tibble(data_raw)


# Add latitude and longitude
latlon <- data_raw %>% select(`LAT LON TOGETHER (DECIMAL DEG)`) %>%
  separate(`LAT LON TOGETHER (DECIMAL DEG)`,c("latitude","longitude"),", ")

data_raw <- data_raw %>% rename(latitude=`LAT LON TOGETHER (DECIMAL DEG)`)
data_raw$latitude <- latlon$latitude
data_raw$longitude <- latlon$longitude

# Add DOI
data_raw$Publication <- "10.1073/pnas.0405147101,10.1111/j.1523-1739.2004.00227.x"

# Add yield

# hand-pollination (receiving augmented pollen to measure
# production with abundant cross-pollination)
# and control (unmanipulated to measure production under ambient pollination)

yield_raw <-
  read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/87_88_TaylorRickettsCoffee_2001_2002/Ricketts yield data for OBServ.xls",
             sheet = "data")

seed_mass <- yield_raw %>% select(-`fruit set`) %>%
  spread(key = Treatment, value = `seed mass`)

fruit_set <- yield_raw %>% select(-`seed mass`) %>%
  spread(key = Treatment, value = `fruit set`)

data_raw$yield <- fruit_set$open
data_raw$yield_units <- "fruit set (%)"
data_raw$yield_treatments_pollen_supplement <- fruit_set$hand

data_raw$yield2 <- seed_mass$open
data_raw$yield2_units <- "wet seed mass per fruit"
data_raw$yield_treatments_pollen_supplement2 <- seed_mass$hand


# Abundance and visitation

abundance_visits <-
  read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/87_88_TaylorRickettsCoffee_2001_2002/Ricketts_data_for_Pasha.xls",
             sheet = "AttribsFincasites") %>% filter(YEAR==2001)

data_raw$observed_pollinator_richness <- abundance_visits$Richness
data_raw$abundance <- abundance_visits$Abundance
data_raw$ab_wildbees <- abundance_visits$Abund_natives
data_raw$ab_honeybee <- data_raw$abundance-data_raw$ab_wildbees


# Visits are given in the datasheet as "visits per 100 flowers and 20 min"
data_raw$visitation_rate <- (3)*abundance_visits$`all bees visitation rate`
data_raw$visitation_rate_units <- "visits per 100 flowers and hour"
data_raw$visit_honeybee <- (3)*(
  abundance_visits$`all bees visitation rate`-abundance_visits$`native visitation rate`)
data_raw$visit_wildbees <- (3)*abundance_visits$`native visitation rate`

# Sampling effort

data_raw$total_sampled_time <- 10*abundance_visits$SAMPLES

# Harmonize yield units

data_raw$yield <- 100*data_raw$yield
#data_raw$yield_treatments_no_pollinators <- 100*data_raw$yield_treatments_no_pollinators
data_raw$yield_treatments_pollen_supplement <- 100*data_raw$yield_treatments_pollen_supplement

###############################
# FIELD LEVEL DATA
###############################

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data_raw, "Processing_files/Datasets_storage/field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
#setwd(dir_ini)


###########################
# INSECT SAMPLING
###########################

insect_sampling_aux <- read_csv("Processing_files/Datasets_storage/insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")

sampling <- abundance_visits %>% select(FINCASITES,SAMPLES) %>% rename(site_id=FINCASITES)

insect_sampling <- insect_sampling_aux %>%
  left_join(sampling,by="site_id") %>%
  mutate(total_sampled_time=10*SAMPLES,total_sampled_flowers=250*SAMPLES) %>%
  select(-SAMPLES)


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
#setwd(dir_ini)
