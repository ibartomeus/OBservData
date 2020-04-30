
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Reemer_Kleijn_2010_Apple
##########################

data_raw <- read_csv("Individual CSV/Reemer_Kleijn_2010_Apple.csv")

# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

# There should be 6 sites/fields. However, there are only 5 site names.
# 

data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_",data_raw_without_NAs$Year_of_study,"_Apple"),
  site_id = data_raw_without_NAs$site,
  crop = "Malus domestica",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "Netherlands",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = 4,
  sampling_end_month = 5,
  sampling_year = data_raw_without_NAs$Year_of_study,
  field_size = NA,
  yield=NA,
  yield_units=NA,
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

# #########################
# Adding credit, Publication and contact

data.site$Publication <- NA
data.site$Credit  <- "David Kleijn"
data.site$Email_contact <- "David.Kleijn@wur.nl"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(site,round,row,observation_location,sample_num, names(data_raw[30:ncol(data_raw)]))


# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]

data_raw_g <- data_raw_obs %>% select(-sample_num)

data_raw_gather <- data_raw_g %>% gather(-site,key = "Organism_ID", value = 'Abundance', !contains("site"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# Each orchard was surveyed twice each
# year. Flower visitors were surveyed using a single transect
# between two rows of trees along the length of each orchard; the
# transect was subdivided into 25 m long plots 
# (mean number of plots per orchard ± SE: 8.5 ± 1.0 for apple and 9.7 ± 0.5 for pear).
# In each plot all flower visitors observed on apple or pear
# flowers during a 10-min period

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0)

insect_sampling <- tibble(
  study_id = "Reemer_Kleijn_2010_Apple",
  site_id = data_raw_gather$site,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "transects",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "Two surveys were conducted in several (25 m long) transects per field (mean number of transects ± SE: 8.5 ± 1.0 for apple and 9.7 ± 0.5 for pear), 10-min per transect"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Reemer_Kleijn_2010_Apple.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_raw_gather <-  data_raw_gather %>%
  group_by(site,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_raw_gather %>% rename(site_id=site) %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are ""bumblebees"      "honeybees"    "other_wild_bees" "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other_flies=0,lepidoptera=0,beetles=0,
                                          non_bee_hymenoptera=0,other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>% rename(site_id=site) %>%
  select(site_id,Organism_ID,Abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=Abundance)

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
  total_sampled_area = NA,
  total_sampled_time = NA,
  visitation_rate_units = NA,
  visitation_rate = NA,
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
write_csv(field_level_data, "field_level_data_Reemer_Kleijn_2010_Apple.csv")
setwd(dir_ini)

