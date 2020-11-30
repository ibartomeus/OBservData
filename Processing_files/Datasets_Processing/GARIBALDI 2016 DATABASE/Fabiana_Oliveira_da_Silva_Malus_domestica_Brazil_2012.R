# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Processing_files/Datasets_Processing/GARIBALDI 2016 DATABASE/Database_Science.txt",sep = " ")


# Malus_domestica_Brazil_2012

# Load data
data_raw <- read_excel("Processing_files/Datasets_Processing/GARIBALDI 2016 DATABASE/Bahia_apple/Metanalise_tablePOLINFRUT_EDIT_OBSERV.xls")
data.coordinates <- read.xlsx("Processing_files/Datasets_Processing/GARIBALDI 2016 DATABASE/Bahia_apple/Coordenadas_Maçã_Graus_decimais.xlsx")
data.yield <- read_excel("Processing_files/Datasets_Processing/GARIBALDI 2016 DATABASE/Bahia_apple/PRODUÇÃO EM KG- LOTES EM QUE FOI REALIZADO TRABALHOS COM POLINIZAÇÃO (2).xls")

# Tranforms date to proper units

data_raw$Date...44 <- as.Date(data_raw$Date...44)
data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$Date...44, format="%Y/%m/%d"),"%m"))

# Select year

data_raw <- data_raw[grepl("2012",data_raw$Site_name,ignore.case = TRUE),]
data_raw %>% group_by(Site_name,Crop_variety) %>% count()

# Select variety

data_raw <- data_raw[grepl("Eva",data_raw$Crop_variety,ignore.case = TRUE),]



# Select data
data.site <- data_raw[,c(1,2,3,14,15,23)] %>%
  rename(country =  Country,crop=Crop_species,site_id=Site_name,
         variety=Crop_variety,
         field_size=Site_size,plant_density=Crop_plant_density) %>% unique()

# Fix Rows info

data.site <- data.site[c(1,3:7),]


# Add latitude longitude

data.coordinates <-  rename(data.coordinates,site_id=Lote,longitude=Long,latitude=Lat)
data.site <- data.site %>% left_join(data.coordinates,by="site_id")


# Add sites' information

data.site$study_id <- "Fabiana_Oliveira_da_Silva_Malus_domestica_Brazil_2012"
data.site$country <- "Brazil"
data.site$management <- "conventional"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_year <- 2012

data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

data.site$Publication <- "10.1126/science.aac7287; 10.26786/1920-7603%282014%2926"
data.site$Credit <- "Fabiana Oliveira da silva; Jeferson Gabriel da Encarnacao Coutinho; Blandina Felipe Viana"
data.site$Email_contact <- "fabia714@gmail.com; jeferson.gabriel@gmail.com; blande.viana@gmail.com "



# Sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)
data_raw_aux <- data_raw[,c(3,44,112)]

for (i in sites){

  data.site$sampling_start_month[data.site$site_id==i] <-
    data_raw_aux %>% filter(Site_name==i) %>%
    select(month_of_study) %>% min()

  data.site$sampling_end_month[data.site$site_id==i] <-
    data_raw_aux %>% filter(Site_name==i) %>%
    select(month_of_study) %>% max()
}

###############################
# YIELD
###############################


data.yield_adapt <- data.yield %>% mutate(GLEBA = str_replace(GLEBA," ","_"),
                                          GLEBA = paste0("Glebe",GLEBA),
                                          site_id=paste("Plot",lote,ANO,GLEBA,sep="_"),
                                          yield_units = "kg") %>%
                      rename(yield=`Peso total dos frutos (kg) em cada lote`) %>%
                      select(site_id,yield, yield_units)


data.site <- data.site %>% left_join(data.yield_adapt,by="site_id")

data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA



###########################
# SAMPLING DATA OBSERVATIONS
###########################

which(names(data_raw)=="no_flowers")

data_raw_obs_ini_ob <- data_raw[,c(3,14,52,46:51)] %>%
  rename(site_id=Site_name,"Apis mellifera"=Apis_mellifera...46)

data_raw_obs_ini_ob %>% group_by(site_id,Crop_variety) %>% count()

# Gather abundance info

data_raw_obs_ob <- data_raw_obs_ini_ob %>% gather(Organism_ID,abundance,c(4:9)) %>%
  filter(abundance>0) %>% select(-Crop_variety)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms_ob <- select(data_raw_obs_ob,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild_ob <- list_organisms_ob %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild_ob %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

 list_organisms_guild_ob$Guild[list_organisms_guild_ob$Organism_ID=="Melipona quadrifasciata anthidioides"] <- "other_wild_bees"

# Sanity Checks
list_organisms_guild_ob %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild_ob <- data_raw_obs_ob %>% left_join(list_organisms_guild_ob, by = "Organism_ID")


#######################
# INSECT SAMPLING OBSERVATIONS
#######################


# Remove entries with zero abundance
data_obs_guild_ob  <- data_obs_guild_ob  %>% filter(abundance>0)

sampling_rounds_ob <- data_raw_obs_ini_ob %>% group_by(site_id,Crop_variety) %>% count() %>%
  mutate(n=n/8)

data_obs_guild_ob <- data_obs_guild_ob %>% left_join(sampling_rounds_ob,by="site_id")

insect_sampling_ob <- tibble(
  study_id = "Fabiana_Oliveira_da_Silva_Malus_domestica_Brazil_2012",
  site_id = data_obs_guild_ob$site_id,
  pollinator = data_obs_guild_ob$Organism_ID,
  guild = data_obs_guild_ob$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild_ob$abundance,
  total_sampled_area = NA,
  total_sampled_time = 5*8*data_obs_guild_ob$n,
  total_sampled_flowers = 50*8*data_obs_guild_ob$n,
  Description = "In each orchad, visitors of four pairs of trees were observed for five minutes each, for a total of 20 minutes of sampling. Sampling was further repeated on at least two dates")


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_ob, "Processing_files/Datasets_storage/insect_sampling_Fabiana_Oliveira_da_Silva_Malus_domestica_Brazil_2012.csv")
#setwd(dir_ini)


#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild_ob %>% select(-Crop_variety) %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "honeybees"    "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,bumblebees=0,other_flies=0,
                                          lepidoptera=0,non_bee_hymenoptera=0,other=0,
                                          syrphids=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild_ob %>% select(-Crop_variety) %>%
  select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>%
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

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
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="only bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################
# TOTAL VISITATION RATE
###############################

# We assumed that sampling four pairs of adjacent trees took 5 minutes per pair (20 min total).

sampling_rounds_ob <- sampling_rounds_ob %>% mutate(total_time=20*n,total_flowers=50*8*n)

# Sites' observations
visits_aux <- abundance_aux

visits_aux <- visits_aux %>% left_join(sampling_rounds_ob, by = "site_id")

visitation_rate <- tibble(
  site_id=visits_aux$site_id,
  total_sampled_area = NA,
  total_sampled_time = visits_aux$total_time,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = 60*100*visits_aux$total/visits_aux$total_time/visits_aux$total_flowers,
  visit_honeybee = 60*100*visits_aux$honeybees/visits_aux$total_time/visits_aux$total_flowers,
  visit_bombus = 60*100*visits_aux$bumblebees/visits_aux$total_time/visits_aux$total_flowers,
  visit_wildbees = 60*100*visits_aux$other_wild_bees/visits_aux$total_time/visits_aux$total_flowers,
  visit_syrphids = 60*100*visits_aux$syrphids/visits_aux$total_time/visits_aux$total_flowers,
  visit_humbleflies = 60*100*visits_aux$humbleflies/visits_aux$total_time/visits_aux$total_flowers,
  visit_other_flies = 60*100*visits_aux$other_flies/visits_aux$total_time/visits_aux$total_flowers,
  visit_beetles = 60*100*visits_aux$beetles/visits_aux$total_time/visits_aux$total_flowers,
  visit_lepidoptera = 60*100*visits_aux$lepidoptera/visits_aux$total_time/visits_aux$total_flowers,
  visit_nonbee_hymenoptera = 60*100*visits_aux$non_bee_hymenoptera/visits_aux$total_time/visits_aux$total_flowers,
  visit_others = 60*100*visits_aux$other/visits_aux$total_time/visits_aux$total_flowers,
)

data.site <- data.site %>% left_join(visitation_rate, by = "site_id")

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
  plant_density=data.site$plant_density/10000,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  richness_restriction = data.site$richness_restriction,
  abundance = data.site$total,
  ab_honeybee = data.site$honeybees,
  ab_bombus = data.site$bumblebees,
  ab_wildbees = data.site$other_wild_bees,
  ab_syrphids = data.site$syrphids,
  ab_humbleflies= data.site$humbleflies,
  ab_other_flies= data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others = data.site$other,
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  visitation_rate_units = data.site$visitation_rate_units,
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = data.site$visit_honeybee,
  visit_bombus = data.site$visit_bombus,
  visit_wildbees = data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Fabiana_Oliveira_da_Silva_Malus_domestica_Brazil_2012.csv")
#setwd(dir_ini)

# COMMENTS
# We assumed that sampling four pairs of adjacent trees takes 5 minutes per round.

