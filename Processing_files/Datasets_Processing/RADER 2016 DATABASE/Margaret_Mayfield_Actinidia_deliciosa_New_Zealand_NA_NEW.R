
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")

dir_ini <- getwd()

##########################
#Data: mayfield_NA
##########################

data_raw <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/mayfield_NA.csv")
data_raw <- as_tibble(data_raw)


# Remove columns full of NA's
data_raw_without_NAs <-
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]



############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = "Margaret_Mayfield_Actinidia_deliciosa_New_Zealand_NA",
  site_id = data_raw_without_NAs$site,
  crop = "Actinidia deliciosa",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "New Zealand",
  latitude = NA,
  longitude = NA,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = 1998,
  field_size = NA,
  yield = data_raw_without_NAs$fruitset,
  yield_units= "mean fruit weight",
  yield2= NA,
  yield2_units= NA,
  yield_treatments_no_pollinators= NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2= NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant= NA,
  fruit_weight=  data_raw_without_NAs$fruitset,
  plant_density= NA,
  seeds_per_fruit= NA,
  seeds_per_plant= NA,
  seed_weight= NA
)


data.site <- data.site_aux %>%
  group_by(study_id,site_id,crop,variety,management,country,
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year,yield_units) %>%
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA

#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1017/CBO9780511754821"
data.site$Credit  <- "Margaret M. Mayfield"
data.site$Email_contact <- "m.mayfield@uq.edu.au"

########################
# Variety: TO1 and 2 orchards were growing Matua variety male plants with
# a mix of M52 and M56 females.  I don't have vine resolution on which females
# were M52 or M56.  Variety at TP = Hort16A. GV1 and GV2 variety = Hayward.
data.site$variety[data.site$site_id %in% c("TO1","TO2")] <- "Matua (males), M52/M56 (females)"
data.site$variety[data.site$site_id %in% c("TP1","TP2")] <- "Hort16A"
data.site$variety[data.site$site_id %in% c("GV1","GV2")] <- "Hayward"

########################
# GV1&2 start date: 18th Nov, 1998; End date: 24/11
# TO1&2 start date: 20th Nov, 1998; End date: 9th December 1998
# TP1&2 start date = 21 October 1998, End date: 5th November, 1998
data.site$sampling_start_month[data.site$site_id %in% c("TO1","TO2")] <- 11
data.site$sampling_end_month[data.site$site_id %in% c("TO1","TO2")] <- 12
data.site$sampling_start_month[data.site$site_id %in% c("TP1","TP2")] <- 10
data.site$sampling_end_month[data.site$site_id %in% c("TP1","TP2")] <- 11
data.site$sampling_start_month[data.site$site_id %in% c("GV1","GV2")] <- 11
data.site$sampling_end_month[data.site$site_id %in% c("GV1","GV2")] <- 11

########################
# GPS coordinates:
# GV1 and GV2: -37.880024, 175.254469
# TO1 and TO2: -37.939645, 175.546225
# TP1: -37.812018, 176.320387
# TP2: -37.818614, 176.314836
data.site$latitude[data.site$site_id %in% c("TO1","TO2")] <- -37.939645
data.site$longitude[data.site$site_id %in% c("TO1","TO2")] <- 175.546225
data.site$latitude[data.site$site_id %in% c("TP1")] <- -37.812018
data.site$longitude[data.site$site_id %in% c("TP1")] <- 176.320387
data.site$latitude[data.site$site_id %in% c("TP2")] <- -37.818614
data.site$longitude[data.site$site_id %in% c("TP2")] <- 176.314836
data.site$latitude[data.site$site_id %in% c("GV1","GV2")] <- -37.880024
data.site$longitude[data.site$site_id %in% c("GV1","GV2")] <- 175.254469


###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(site, names(data_raw[30:ncol(data_raw)]))

# Remove NAs
data_raw_obs <-
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


data_raw_gather <- data_raw_obs %>% rename(site_id = site)%>%
  gather(-site_id,key = "Organism_ID", value = 'Abundance', !contains("site_id"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################



# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0,!is.na(Abundance))


insect_sampling <- tibble(
  study_id = "Margaret_Mayfield_Actinidia_deliciosa_New_Zealand_NA",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "observations",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "In each of the six blocks, six female vines were randomly selected. On each vine flower visitors were observed in 10-min intervals at all times of day throughout the 17-day blooming season."
)


insect_sampling$Description[insect_sampling$site_id %in% c("TP1")] <- paste0(insect_sampling$Description[insect_sampling$site_id %in% c("TP1")],
                                                                             " All vines in TP1 were > 500 m and < 1 km from wild vegetation.")
insect_sampling$Description[insect_sampling$site_id %in% c("TP2")] <- paste0(insect_sampling$Description[insect_sampling$site_id %in% c("TP2")],
                                                                             " All vines in TP2 were within 300m of wild vegetation.")
#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Margaret_Mayfield_Actinidia_deliciosa_New_Zealand_NA.csv")
#setwd(dir_ini)


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>%
  select(site_id,Organism_ID,Abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=Abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

abundace_field[,c(2:7)]/min(abundace_field[,c(2:7)][abundace_field[,c(2:7)]>0])

abundace_field[,c(2:7)] <- abundace_field[,c(2:7)]/min(abundace_field[,c(2:7)][abundace_field[,c(2:7)]>0])

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator
}

# Load our estimation for taxonomic resolution

tax_res <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/taxon_table_Rader.csv")
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

#######################################
# VISITATION RATE
#######################################

# Add site observations

data_raw_gather2 <-  data_raw_gather %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


visit_aux <- data_raw_gather2 %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>%
  spread(key=Guild, value=n)

names(visit_aux)

# There are     "beetles" "honeybees" ""bumblebees "non_bee_hymenoptera"
# "" other_flies,     "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(other_wild_bees=0,lepidoptera=0,
                                          other=0,humbleflies=0,total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(2:ncol(visit_aux))])

data.site <- data.site %>% left_join(visit_aux, by = "site_id")



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
  richness_restriction = "None",
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
  visitation_rate_units = "Visits per hour",#"Visits per 10 minute",
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
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Margaret_Mayfield_Actinidia_deliciosa_New_Zealand_NA.csv")
#setwd(dir_ini)

