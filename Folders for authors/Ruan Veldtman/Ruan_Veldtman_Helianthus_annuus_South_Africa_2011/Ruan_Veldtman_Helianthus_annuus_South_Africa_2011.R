# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")


# Sunflowers_SouthAfrica_2011

# Load data
data_raw <- read_excel("SouthAfrica Sunflower/Common_database_SA_Sunflower_2011_scan_collection_and_seed_weights.xlsx",
                        sheet = "Scan sampling")


data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$date, format="%Y/%m/%d"),"%m"))


# Select data 
data.site <- data_raw[,c(1,2,3,11,14,15,16,19,32,33)] %>% 
  rename(site_id=Site,sampling_year=year,latitude=DDS,longitude=DDE,
         variety=crop_variety,
         field_size="Size(ha)",plant_density="crop density(plants/ha)",
         flowerss_per_ha="crop density(flowerss/ha)") %>% unique()


# Fix crop and latitude
data.site$crop <- "Helianthus annuus"
data.site$latitude <- -1*data.site$latitude

# Add sites' information 

data.site$study_id <- "Ruan_Veldtman_Helianthus_annuus_South_Africa_2011"
data.site$management <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA

data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA

data.site$Publication <- "10.1126/science.aac7287"
data.site$Credit <- "Ruan Veldtman & Jonathan Colville"
data.site$Email_contact <- "R.Veldtman@sanbi.org.za"

	

# Sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)
data_raw_aux <- data_raw[,c(14,59)]

for (i in sites){
  
  data.site$sampling_start_month[data.site$site_id==i] <- 
    data_raw_aux %>% filter(Site==i) %>% 
    select(month_of_study) %>% min()
  
  data.site$sampling_end_month[data.site$site_id==i] <- 
    data_raw_aux %>% filter(Site==i) %>% 
    select(month_of_study) %>% max()
}

###############################
# YIELD
###############################
data.yield <- read_excel("SouthAfrica Sunflower/Common_database_SA_Sunflower_2011_scan_collection_and_seed_weights.xlsx",
                         sheet = "seed_weights")

data.yield_adapt <- data.yield %>%
                      rename(site_id=Site,yield=`Weight_of_10_sunflower_heads(=avergae*10)`) %>%
                      select(site_id,yield) %>% mutate(yield=yield/1000) %>%
  group_by(site_id) %>% summarise_all(mean)
                      

data.site <- data.site %>% left_join(data.yield_adapt,by="site_id")

data.site$yield_units <- "Weight of 10 sunflower heads (kg)"
data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA

###########################
# SAMPLING DATA TRANSECTS
###########################

data_raw_obs_ini <- data_raw[,c(14,27,33,36:58)] %>% 
  rename(site_id=Site)

data_raw_obs_ini %>% group_by(site_id) %>% count()

# Gather abundance info

data_raw_obs <- data_raw_obs_ini %>% gather(Organism_ID,abundance,c(4:26)) %>%
  filter(abundance>0) %>% select(-`time(secs)`,-`crop density(flowerss/ha)`)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
x <- list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[grepl("Lasioglossum",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Lycaenidae",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Hesperiidae",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Hypolimnas",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("large_hover",list_organisms_guild$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild$Guild[grepl("Apis_millifera",list_organisms_guild$Organism_ID,ignore.case = T)] <- "honeybees"
list_organisms_guild$Guild[grepl("Acraea_horta",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Astylus_atromaculatus",list_organisms_guild$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild$Guild[grepl("Colotis",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("African Monarch butterfly",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Lygus",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild$Guild[grepl("Macroglossum_trochilus",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Muscid",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_flies"
list_organisms_guild$Guild[grepl("Noctuidae",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Precis_oenone",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild$Guild[grepl("Asarkina",list_organisms_guild$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild$Guild[grepl("Sphecidae_wasp",list_organisms_guild$Organism_ID,ignore.case = T)] <- "non_bee_hymenoptera"
list_organisms_guild$Guild[grepl("Tenebrionidae",list_organisms_guild$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild$Guild[grepl("Tetraloniella_braunsiana",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Xylocopa",list_organisms_guild$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Danus_chrysippus_aegyptius",list_organisms_guild$Organism_ID,ignore.case = T)] <- "lepidoptera"



#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")



############################
# INSECT SAMPLING TRANSECTS
############################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

sampling_rounds <- data_raw_obs_ini %>% select(site_id,`time(secs)`) %>%
  mutate(`time(secs)`=0.25*`time(secs)`/60) %>% #`time(secs)` refers to 4 transects apparently
  group_by(site_id) %>% summarize_all(sum) %>% rename(time=`time(secs)`)

data_obs_guild <- data_obs_guild %>% left_join(sampling_rounds,by="site_id")

insect_sampling <- tibble(
  study_id = "Ruan_Veldtman_Helianthus_annuus_South_Africa_2011",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "sweep net",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = data_obs_guild$time,
  total_sampled_flowers = 400,
  Description = "On each field study site, 100 flower heads in each of four parallel transects (total 400 flower heads) were surveyed in the morning and afternoon")

###########################
# SAMPLING DATA OBSERVATIONS
###########################

# Voucher specimens for all insect flower visitors that touched the 
# reproductive structures of surveyed sunflower heads were collected. 
# From this, all bee specimens were identified to the lowest possible taxonomic level.

data_raw_obs_ini_ob <- read_excel("SouthAfrica Sunflower/Common_database_SA_Sunflower_2011_scan_collection_and_seed_weights.xlsx",
                               sheet = "collection_samples") %>%
  rename(site_id = `CODE(A=away from natural; N=near natural; AM=morning; PM=afternoon; S=scan)`)

# Transform site labels
data_raw_obs_ini_ob$site_id <- substr(data_raw_obs_ini_ob$site_id, start = 1, stop = 2)


data_raw_obs_ini_ob %>% group_by(site_id) %>% count()

# Gather abundance info

data_raw_obs_ob <- data_raw_obs_ini_ob %>% gather(Organism_ID,abundance,c(2:40)) %>%
  filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms_ob <- select(data_raw_obs_ob,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild_ob <- list_organisms_ob %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild_ob %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild_ob$Guild[grepl("Lasioglossum",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild_ob$Guild[grepl("Lycaenidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Hesperiidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Hypolimnas",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("large_hover",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild_ob$Guild[grepl("Apis_millifera",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "honeybees"
list_organisms_guild_ob$Guild[grepl("Acraea_horta",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Astylus_atromaculatus",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild_ob$Guild[grepl("Colotis",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("African Monarch butterfly",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Lygus",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild_ob$Guild[grepl("Macroglossum_trochilus",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Muscid",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_flies"
list_organisms_guild_ob$Guild[grepl("Noctuidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Precis_oenone",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Asarkina",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "syrphids"
list_organisms_guild_ob$Guild[grepl("Sphecidae_wasp",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "non_bee_hymenoptera"
list_organisms_guild_ob$Guild[grepl("Tenebrionidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild_ob$Guild[grepl("Tetraloniella_braunsiana",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild_ob$Guild[grepl("Xylocopa",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_wild_bees"
list_organisms_guild_ob$Guild[grepl("Danus_chrysippus_aegyptius",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"

list_organisms_guild_ob$Guild[grepl("bee.red.abdomen.sp1",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- NA
list_organisms_guild_ob$Guild[grepl("Bombyliidae_sp.1",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_flies"
list_organisms_guild_ob$Guild[grepl("brown_skipper_sp1.",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Caliphoridae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_flies"
list_organisms_guild_ob$Guild[grepl("Cetoniidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild_ob$Guild[grepl("Chamaesphecia_anthraciformis",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Chrysoperla",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild_ob$Guild[grepl("Cleridae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild_ob$Guild[grepl("Coeliades_pisistratus",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Colias",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Fulgoridae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild_ob$Guild[grepl("Lace_bug",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other"
list_organisms_guild_ob$Guild[grepl("ladybird",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "beetles"
list_organisms_guild_ob$Guild[grepl("Mylothris",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"
list_organisms_guild_ob$Guild[grepl("Sarcophagidae",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "other_flies"
list_organisms_guild_ob$Guild[grepl("Vanessa_cardui",list_organisms_guild_ob$Organism_ID,ignore.case = T)] <- "lepidoptera"



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
  mutate(n=n/6)

data_obs_guild_ob <- data_obs_guild_ob %>% left_join(sampling_rounds_ob,by="site_id")

insect_sampling_ob <- tibble(
  study_id = "Ruan_Veldtman_Helianthus_annuus_South_Africa_2011",
  site_id = data_obs_guild_ob$site_id,
  pollinator = data_obs_guild_ob$Organism_ID,
  guild = data_obs_guild_ob$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild_ob$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "Voucher specimens for all insect flower visitors that touched the reproductive structures of surveyed sunflower heads were collected. From this, all bee specimens were identified to the lowest possible taxonomic level.")


insect_sampling_total <- bind_rows(insect_sampling,insect_sampling_ob)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_total, "insect_sampling_Ruan_Veldtman_Helianthus_annuus_South_Africa_2011.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Using transects counts

data_obs_guild_2 <- data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>% filter(!is.na(Guild)) %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are "beetles"             "honeybees"           "lepidoptera"        
# "non_bee_hymenoptera" "other"               "other_flies"         "other_wild_bees"    
# "syrphids"             

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,humbleflies=0,total=0)

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
percentage_species_morphos <- 0.9

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

visits_aux <- abundance_aux

visits_aux <- visits_aux %>% left_join(sampling_rounds, by = "site_id")

visitation_rate <- tibble(
  site_id=visits_aux$site_id,
  total_sampled_area = NA,
  total_sampled_time = visits_aux$time,
  visitation_rate_units = "visits per 100 sunflowers heads and hour",
  visitation_rate = 60*100*visits_aux$total/visits_aux$time/400,
  visit_honeybee = 60*100*visits_aux$honeybees/visits_aux$time/400,
  visit_bombus = 60*100*visits_aux$bumblebees/visits_aux$time/400,
  visit_wildbees = 60*100*visits_aux$other_wild_bees/visits_aux$time/400,
  visit_syrphids = 60*100*visits_aux$syrphids/visits_aux$time/400,
  visit_humbleflies = 60*100*visits_aux$humbleflies/visits_aux$time/400,
  visit_other_flies = 60*100*visits_aux$other_flies/visits_aux$time/400,
  visit_beetles = 60*100*visits_aux$beetles/visits_aux$time/400,
  visit_lepidoptera = 60*100*visits_aux$lepidoptera/visits_aux$time/400,
  visit_nonbee_hymenoptera = 60*100*visits_aux$non_bee_hymenoptera/visits_aux$time/400,
  visit_others = 60*100*visits_aux$other/visits_aux$time/400,
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
write_csv(field_level_data, "field_level_data_Ruan_Veldtman_Helianthus_annuus_South_Africa_2011.csv")
setwd(dir_ini)

