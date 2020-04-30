
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Brittain_Klein_2008
##########################

data_raw <- read_csv("Individual CSV/Brittain_Klein_2008.csv")

# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_",data_raw_without_NAs$Year_of_study),
  site_id = data_raw_without_NAs$site,
  crop = "Prunus dulcis",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "USA",
  latitude = NA,
  longitude = NA,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = data_raw_without_NAs$month_of_study,
  sampling_end_month = data_raw_without_NAs$month_of_study,
  sampling_year = data_raw_without_NAs$Year_of_study,
  field_size = NA,
  yield= 100 * data_raw_without_NAs$fruitset,
  yield_units="fruitset (%)",
  yield2= NA,
  yield2_units= NA,
  yield_treatments_no_pollinators= NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2= NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant= NA,
  fruit_weight=  NA,
  plant_density= NA,
  seeds_per_fruit= NA,
  seeds_per_plant= NA,
  seed_weight= NA
)


# Fix sampling months

sites <- unique(data.site_aux$site_id)

for (i in sites) {
  
  data.site_aux$sampling_start_month[data.site_aux$site_id==i] <- 
    data.site_aux %>% filter(site_id==i) %>% 
    select(sampling_start_month) %>% min()
  
  data.site_aux$sampling_end_month[data.site_aux$site_id==i] <- 
    data.site_aux %>% filter(site_id==i) %>% 
    select(sampling_end_month) %>% max()

}
data.site <- data.site_aux %>% 
  group_by(study_id,site_id,crop,variety,management,country,
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year,yield_units) %>% 
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA

############################################################

# Convert Latitude/Longitude from degrees min sec to decimal
# 
# chd = substr(data.site$latitude, 3, 3)[1]
# chm = substr(data.site$latitude, 6, 6)[1]
# chs = substr(data.site$latitude, 9, 10)[1]
# 
# cd = char2dms(data.site$latitude,chd=chd,chm=chm,chs=chs)
# data.site$latitude <- as.numeric(cd)
# 
# chd = substr(data.site$longitude, 3, 3)[1]
# chm = substr(data.site$longitude, 5, 5)[1]
# chs = substr(data.site$longitude, 8, 9)[1]
# 
# cd = char2dms(data.site$longitude,chd = chd,chm = chm,chs = chs)
# data.site$longitude <- as.numeric(cd)

#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1111/gcb.12043"
data.site$Credit  <- "Claire Brittain, Claire Kremen and Alexandra-Maria Klein"
data.site$Email_contact <- "alexandra.klein@nature.uni-freiburg.de"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(site,round,row,observation_location, names(data_raw[30:ncol(data_raw)]))

# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


data_raw_gather <- data_raw_obs %>% gather(-site,key = "Organism_ID", value = 'Abundance', !contains("site"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# In each orchard,
# visitors to almond flowers were observed and recorded
# during 20 s observation periods. Observations were conducted
# on five trees at the edge (outer row) of the orchards
# and five trees 100 m inside the orchard (or 50 m inside in four
#                                          smaller orchards, mean tree height 6 m). Each tree was visually
# split into four sections: top interior, top exterior, bottom
# interior, and bottom exterior (Fig. 1). At each tree on a given
# day, each of the four sections was observed twice. Each tree
# section was observed successively in a random order and the
# number of flowers under observation was noted.

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0)

insect_sampling <- tibble(
  study_id = "Brittain_Klein_2008",
  site_id = data_raw_gather$site,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "observation",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = 3*10*4*2*20/60,
  total_sampled_flowers = 3*10*4*2*19,
  Description = "Visitation rates: 3 surveys per field, 10 trees per survey, 4 sections per tree, 2 observations per section, and 20 seconds per observation and group of flowers (mean 19 flowers)."
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Brittain_Klein_2008.csv")
setwd(dir_ini)

#######################################
# VISITATION RATE
#######################################

# Add site observations

data_raw_gather <-  data_raw_gather %>% rename(site_id=site) %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


visit_aux <- data_raw_gather %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>% 
  spread(key=Guild, value=n)

names(visit_aux)

# There are "beetles" ""bumblebees"      "honeybees"   "lepidoptera"     "other_flies" 
# "other_wild_bees" "syrphids" humbleflies non_bee_hymenoptera other

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(2:ncol(visit_aux))])

data.site <- data.site %>% left_join(visit_aux, by = "site_id")


###############################
# SPECIES RICHNESS
###############################


#Estimate species richness from visits

abundace_field <- data_raw_gather %>%
  select(site_id,Organism_ID,Abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=Abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
#Set non integer abundances to 1. Otherwise, the iNet code that is below will crash.
abundace_field[,2:ncol(abundace_field)][abundace_field[,2:ncol(abundace_field)] > 0] <- 1
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}

# Load our estimation for taxonomic resolution

tax_res <- read_csv("taxon_table_Rader.csv")
#Mutate pollinator labels to match those of taxon table
tax_estimation <- insect_sampling %>% mutate(pollinator=str_replace(pollinator,"_"," ")) %>%
  left_join(tax_res, by="pollinator")
tax_estimation %>% group_by(rank) %>% count()

percentage_species_morphos <- 
  sum(tax_estimation$rank %in% c("morphospecies","species"))/nrow(tax_estimation)


richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}
# Since the prior chao estimation makes no sense, we remove it
richness_aux$other_pollinator_richness <- NA
richness_aux$other_richness_estimator_method <- NA

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
  abundance = NA,
  ab_honeybee = NA,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles = NA,
  ab_lepidoptera = NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = 3*10*4*2*20/60,
  visitation_rate_units = "visits per flower and second",
  visitation_rate = data.site$total,
  visit_honeybee = data.site$honeybees,
  visit_bombus = data.site$bumblebees,
  visit_wildbees = data.site$other_wild_bees,
  visit_syrphids = data.site$syrphids,
  visit_humbleflies = data.site$humbleflies,
  visit_other_flies = data.site$other_flies,
  visit_beetles = data.site$beetles,
  visit_lepidoptera = data.site$lepidoptera,
  visit_nonbee_hymenoptera = data.site$non_bee_hymenoptera,
  visit_others = data.site$other,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Brittain_Klein_2008.csv")
setwd(dir_ini)
