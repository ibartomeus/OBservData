
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")

dir_ini <- getwd()

##########################
#Data: mayfield_NA
##########################

data_raw <- read_csv("Individual CSV/mayfield_NA.csv")
data_raw <- as_tibble(data_raw)


# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]



############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_NA"),
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
  sampling_year = NA,
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

data.site$Publication <- "Connectivity for Conservation (Cambridge Univ Press, Cambridge, UK)"
data.site$Credit  <- "Taylor H. Ricketts, Neal M. Williams, and Margaret M. Mayfield"
data.site$Email_contact <- "m.mayfield@uq.edu.au"

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

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################



# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0,!is.na(Abundance))


insect_sampling <- tibble(
  study_id = "mayfield_NA",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "observations",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "In each of the six blocks, six female vines were randomly selected. On each vine flower visitors were observed in 10-min intervals at all times of day throughout the 17-day blooming season"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_mayfield_NA.csv")
setwd(dir_ini)


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

abundace_field[abundace_field>0] <- 1

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}

richness_aux <- abundace_field %>% select(site_id, r_obser)
richness_aux <- richness_aux %>% rename(pollinator_richness=r_obser) %>%
  mutate(richness_estimator_method="observed")

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
  pollinator_richness = data.site$pollinator_richness,
  richness_estimator_method = data.site$richness_estimator_method,
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
  visitation_rate_units = NA,
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
write_csv(field_level_data, "field_level_data_mayfield_NA.csv")
setwd(dir_ini)

