# load libraries
library(tidyverse)
library("iNEXT")
#library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")


# Brassica_napus_Brazil_2011

# Load data
data_raw <- read.xlsx("doSul_Canola/Tabela Rede Canola 24.07.2013 revisado yield.xlsx",startRow = 3)

# Tranforms date to proper units

data_raw$Date <- openxlsx::convertToDate(data_raw$Date)
data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$Date, format="%Y/%m/%d"),"%m"))


# Select data 
data.site <- data_raw[,c(1,2,3,5,6,13,14,20)] 


# Fix latitude longitude

data.site$Latitude[data.site$Site_name=="L3 Karlec"] <- "S28°13769"
data.site$Longitude[data.site$Site_name=="L3 Karlec"] <- "W054º34718"
data.site$Latitude[data.site$Site_name=="L 6 Sr. Donadel"] <- "S28°13342"
data.site$Longitude[data.site$Site_name=="L 1 Snitowski"] <- "W054°31870"
data.site <-data.site %>% unique()

data.site$Latitude <- str_replace(data.site$Latitude,"°",".")
data.site$Latitude <- parse_lat(data.site$Latitude)

data.site$Longitude <- str_replace(data.site$Longitude,"W0","W")
data.site$Longitude <- str_replace(data.site$Longitude,"°",".")
data.site$Longitude <- str_replace(data.site$Longitude,"º",".")
data.site$Longitude <- parse_lon(data.site$Longitude)




data.site <- data.site %>% rename(country =  Country,crop=Crop_species,site_id=Site_name,
                    latitude = Latitude,longitude = Longitude,variety=Crop_variety,
                    field_size=Site_size,plant_density=Crop_plant_density)

# Add sites' information 

data.site$study_id <- "Brassica_napus_Brazil_2011"
data.site$country <- "Brazil"
data.site$management <- "conventional"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_year <- 2011

data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

data.site$Publication <- "J. Pollinat. Ecol., 12 (2014), pp. 15-21"
data.site$Credit <- "Betina Blochtein & Patrícia Nunes-Silva & Rosana Halinski de Oliveira"
data.site$Email_contact <- "patriciabiene@gmail.com; betinabl@pucrs.br; rosana.oliveira@pucrs.br"

# Sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)
data_raw_aux <- data_raw[,c(3,42,95)]

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

which(names(data_raw)=="Yield_quantity")

yield <- data_raw[,c(3,67)] %>% mutate(yield_units = "kg") %>% unique() %>% 
  rename(site_id=Site_name)

data.site <- data.site %>% left_join(yield,by="site_id") %>% rename(yield=Yield_quantity)

data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA

###########################
# SAMPLING DATA
###########################

which(names(data_raw)=="Lepidoptera")

data_raw_obs_ini <- data_raw[,c(3,42:64)] %>% 
  rename(site_id=Site_name)

data_raw_obs_ini %>% group_by(site_id,Date,Time) %>% count()

# Gather abundance info

data_raw_obs <- data_raw_obs_ini %>% gather(Organism_ID,abundance,c(6:24)) %>%
  filter(abundance>0) %>% select(-Date,-Time,-Insects,-`no_flowers.(inflorescences)`)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Augochlora.spp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Augochlorella.sp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Augochloropsis.spp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Bombus.pauloensis"] <- "bumblebees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Exomalopsis.spp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Neocorynura.sp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Other.Hymenoptera"] <- "non_bee_hymenoptera"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Plebeia.emerina"] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Plebeia.nigriceps"] <- "other_wild_bees"

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Psaenythia.sp."] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Schwarziana.quadripunctata"] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Tetragonisca.fiebrigi"] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Trigona.spinipes"] <- "other_wild_bees"
list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Xylocopa.sp."] <- "other_wild_bees"


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
  study_id = "Brassica_napus_Brazil_2011",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "sweep net",
  abundance = data_obs_guild$abundance,
  total_sampled_area = 50*6*9,
  total_sampled_time = 5*6*9,
  total_sampled_flowers = NA,
  Description = "In each field abundance of floral visitors was analysed by sweep-netting all visitors along six 25 m long and 2 m wide transects for five minutes each, for a total of 30 minutes of sampling. Sampling was further repeated on at least three dates during the main flowering period")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Brassica_napus_Brazil_2011.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "beetles"             "bumblebees"          "honeybees"          
# "lepidoptera"         "non_bee_hymenoptera" "other"               "other_flies"        
# "other_wild_bees"    

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(syrphids=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
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
percentage_species_morphos <- 0.3

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

richness_aux$other_pollinator_richness/6

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################
# TOTAL VISITATION RATE
###############################

#In Garibaldi 2016, flower-visitor density refers to # flower visitors in 100 flowers

visit_aux <- data_raw_obs_ini[,1:5] %>% mutate(Time = 15) %>% 
  filter(!is.na(Insects)) %>% select(-Date) %>%
  group_by(site_id) %>% summarise_all(sum) 

data.site$visitation_rate_units <-  "visits per 100 flowers and hour"

data.site$visitation_rate <- 
  100*60*visit_aux$Insects/visit_aux$`no_flowers.(inflorescences)`/visit_aux$Time


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
  total_sampled_area = 50*6*9,
  total_sampled_time = 5*6*9,
  visitation_rate_units = data.site$visitation_rate_units,
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = NA,
  visit_bombus = NA,
  visit_wildbees = NA,
  visit_syrphids = NA,
  visit_humbleflies = NA,
  visit_other_flies = NA,
  visit_beetles = NA,
  visit_lepidoptera = NA,
  visit_nonbee_hymenoptera = NA,
  visit_others = NA,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Brassica_napus_Brazil_2011.csv")
setwd(dir_ini)

# We assumed that all dipterans belong to other_flies guild
# Results for flower-visitor density (no. flower visitors in 100
# flowers), and flower-visitor richness (no. species in 30 minutes) are different
# from those in Garibaldi's supplementary material (Science, 2016)


#######################################
# FLOWER VISITOR RICHNESS: A COMPARISON
#######################################

data_obs_test <- data_raw[,c(3,46:64)] %>% 
  rename(site_id=Site_name)

dummy_var <- tibble(dummy=rep(NA,54*6))

count_i <- 0
for (i in 1:nrow(dummy_var)){
  if (i%%6==1){
    count_i <- count_i+1
    dummy_var$dummy[i] <- count_i
  }else{dummy_var$dummy[i] <- count_i}
}

data_obs_test2 <- bind_cols(dummy_var,data_obs_test)

x <- data_obs_test2 %>% group_by(site_id,dummy) %>% count()

abundace_field_test <- data_obs_test2 %>%
  group_by(site_id,dummy) %>% summarise_all(sum) %>% select(-dummy)

abundace_field_test$r_obser <-  0
abundace_field_test$r_chao <-  0

for (i in 1:nrow(abundace_field_test)) {
  x <- as.numeric(abundace_field_test[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field_test$r_obser[i] <-  chao$Observed
  abundace_field_test$r_chao[i] <-  chao$Estimator 
}

abundace_field_test %>% select(site_id,r_obser) %>% group_by(site_id) %>% summarise_all(mean)

science_raw %>% filter(Crop_System=="Brazil Brassica napus 2011") %>% 
  select(Crop_System, Flower_visitor_richness)

