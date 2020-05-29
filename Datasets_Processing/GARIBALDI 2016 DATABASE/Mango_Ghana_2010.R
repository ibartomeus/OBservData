# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")


# Mango_Ghana_2010

# Load data
data_raw <- read.xlsx("Ghana/GhanaMango Master sheet Peter.xlsx",
                       startRow = 3)

# Fix farms names

data_raw$Site_name[grepl("Double T farms",data_raw$Site_name,ignore.case = FALSE)] <- "Double T farms"
data_raw$Site_name[grepl("Enyonam farms",data_raw$Site_name,ignore.case = FALSE)] <- "Enyonam farms"
data_raw$Site_name[grepl("Grace farms",data_raw$Site_name,ignore.case = FALSE)] <- "Grace farms"
data_raw$Site_name[grepl("Modest Step Farm",data_raw$Site_name,ignore.case = FALSE)] <- "Modest Step farms"
data_raw$Site_name[grepl("Prof. Offei farms",data_raw$Site_name,ignore.case = FALSE)] <- "Prof. Offei farms"
data_raw$Site_name[grepl("Prudent Export 2",data_raw$Site_name,ignore.case = FALSE)] <- "Prudent Export 2"

# Tranforms date to proper units

data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$Date, format="%Y/%m/%d"),"%m"))

# Select data 
data.site <- data_raw[,c(1,2,3,4,8,9,16,17,23)] %>% 
  rename(country =  Country,crop=Crop_species,site_id=Site_name,
         variety=Crop_variety,sampling_year=YEAR,latitude=Latitude,longitude=Longitude,
         field_size=Site_size,plant_density=Crop_plant_density) %>% unique()

data.site <- data.site %>% filter(site_id %in% c("Double T farms","Enyonam farms","Grace farms",
                                                 "Modest Step farms","Prof. Offei farms","Prudent Export 2"))

data.site$country <- "Ghana"

# Parse latitude longitude

data.site$latitude <- parse_lat(data.site$latitude)
data.site$longitude <- parse_lon(data.site$longitude)

# Add sites' information 

data.site$study_id <- "Mango_Ghana_2010"
data.site$management <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA

data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

data.site$Publication <- "10.1126/science.aac7287"
data.site$Credit <- "Peter Kwapong"
data.site$Email_contact <- "pkwapong@yahoo.com"

	

# Sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)
data_raw_aux <- data_raw[,c(3,96)]

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
##############################

data.yield <- data_raw[,c(3,71,72)] 
data.yield2 <- data.yield %>% rename(site_id=Site_name,fruits_per_plant = Number.of.fruit.per.tree,
                                     yield=Weight.of.fruits.per.tree) %>%
  mutate(fruit_weight=1000*yield/fruits_per_plant) %>% #(from kg to grams)
  group_by(site_id) %>% summarise_all(mean,na.rm = TRUE) %>% 
  filter(!is.nan(yield))

data.site <- data.site %>% left_join(data.yield2,by="site_id")

data.site$yield <- data.site$yield*data.site$plant_density

data.site$yield_units <- "kg/ha"
data.site$yield2_units <- NA
data.site$yield2 <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_pollen_supplement <- NA

###########################
# SAMPLING DATA TRANSECTS
###########################

which(names(data_raw)=="STINGLESS.BEES")

data_raw_obs_ini <- data_raw[,c(3,64:67)] %>% 
  rename(site_id=Site_name) %>% 
  filter(site_id %in% c("Double T farms","Enyonam farms","Grace farms",
                        "Modest Step farms","Prof. Offei farms","Prudent Export 2"))

data_raw_obs_ini %>% group_by(site_id) %>% count()

# Gather abundance info

data_raw_obs <- data_raw_obs_ini %>% gather(Organism_ID,abundance,c(2:5)) %>%
  filter(abundance>0,!is.na(abundance))


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[grepl("Flies",list_organisms_guild$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild$Guild[grepl("Other",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild$Guild[grepl("OTHER.WILD.BEES",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("STINGLESS.BEES",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_wild_bees"


#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


############################
# INSECT SAMPLING TRANSECTS
############################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

sampling_rounds <- data_raw_obs_ini %>% group_by(site_id) %>% count() %>%
  mutate(n=n/8)

data_obs_guild <- data_obs_guild %>% left_join(sampling_rounds,by="site_id")

insect_sampling <- tibble(
  study_id = "Mango_Ghana_2010",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "sweep net",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = 100*8*data_obs_guild$n,
  Description = "In each field abundance of floral visitors was analysed by sweep-netting all visitors along four pairs of trees. Sampling was further repeated on four dates during the main flowering period")

###########################
# SAMPLING DATA OBSERVATIONS
###########################

which(names(data_raw)=="Apis_mellifera")

data_raw_obs_ini_ob <- data_raw[,c(3,53:57)] %>% 
  rename(site_id=Site_name)%>% 
  filter(site_id %in% c("Double T farms","Enyonam farms","Grace farms",
                        "Modest Step farms","Prof. Offei farms","Prudent Export 2"))

data_raw_obs_ini_ob %>% group_by(site_id) %>% count()

# Gather abundance info

data_raw_obs_ob <- data_raw_obs_ini_ob %>% gather(Organism_ID,abundance,c(2:6)) %>%
  filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms_ob <- select(data_raw_obs_ob,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild_ob <- list_organisms_ob %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild_ob %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild_ob$Guild[grepl("Flies",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild_ob$Guild[grepl("Other",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild_ob$Guild[grepl("Other.wild.bees",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild_ob$Guild[grepl("Stingless.bees",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_wild_bees"



#Sanity Checks
list_organisms_guild_ob %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild_ob <- data_raw_obs_ob %>% left_join(list_organisms_guild_ob, by = "Organism_ID")


#######################
# INSECT SAMPLING OBSERVATIONS
#######################


# Remove entries with zero abundance
data_obs_guild_ob  <- data_obs_guild_ob  %>% filter(abundance>0)

sampling_rounds_ob <- data_raw_obs_ini_ob %>% group_by(site_id) %>% count() %>%
  mutate(n=n/8)

data_obs_guild_ob <- data_obs_guild_ob %>% left_join(sampling_rounds_ob,by="site_id")

insect_sampling_ob <- tibble(
  study_id = "Mango_Ghana_2010",
  site_id = data_obs_guild_ob$site_id,
  pollinator = data_obs_guild_ob$Organism_ID,
  guild = data_obs_guild_ob$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild_ob$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = 100*8*data_obs_guild_ob$n,
  Description = "In each orchad, visitors of four pairs of trees were observed. Sampling was further repeated four times.")


insect_sampling_total <- bind_rows(insect_sampling,insect_sampling_ob)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_total, "insect_sampling_Mango_Ghana_2010.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  bind_rows(data_obs_guild,data_obs_guild_ob) %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are """          "honeybees"       "other"     "syrphids"        
# "other_wild_bees"   

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,bumblebees=0,lepidoptera=0,
                                          non_bee_hymenoptera=0,other_flies=0,
                                          humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild_ob %>%
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
percentage_species_morphos <- 174/771

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################
# TOTAL VISITATION RATE
###############################

# We assumed that sampling four pairs of adjacent trees took 5 minutes per pair (20 min total).

sampling_rounds_ob <- sampling_rounds_ob %>% mutate(total_time=20*n,total_flowers=100*8*n)
# Add site observations

data_obs_guild_2_ob <-  data_obs_guild_ob %>% 
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


visits_aux <- data_obs_guild_2_ob %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(visits_aux)

# There are "other"          "honeybees"   "syrphids"         "other_wild_bees"    

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visits_aux <- visits_aux %>% mutate(beetles=0, bumblebees=0,lepidoptera=0,non_bee_hymenoptera=0,
                                    other_flies=0,humbleflies=0,total=0)
visits_aux[is.na(visits_aux)] <- 0
visits_aux$total <- rowSums(visits_aux[,c(2:ncol(visits_aux))])


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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Mango_Ghana_2010.csv")
setwd(dir_ini)

# We assumed that sampling four pairs of adjacent trees took 5 minutes per pair (20 min total per round).
