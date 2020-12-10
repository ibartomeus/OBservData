
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Winfree_griffin_2011
##########################

data_raw <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/Winfree_griffin_2011.csv")

# Remove columns full of NA's
data_raw_without_NAs <-
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

# There should be 6 sites/fields. However, there are only 5 site names.
#

data.site_aux <- tibble(
  study_id = "Rachael_Winfree_Citrullus_lanatus_USA_2011",
  site_id = data_raw_without_NAs$site,
  crop = "Citrullus lanatus",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "USA",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = data_raw_without_NAs$month_of_study,
  sampling_end_month = data_raw_without_NAs$month_of_study,
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


# Fix sampling months

sites <- unique(data.site_aux$site_id)

for (i in sites){

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

# #########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1111/j.1461-0248.2007.01110.x"
data.site$Credit  <- "Rachael Winfree, Neal M. Williams, Jonathan Dushoff, and Claire Kremen"
data.site$Email_contact <- "rwinfree@rutgers.edu"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(site,round,row,observation_location,sample_num, names(data_raw[30:ncol(data_raw)]))


# Remove NAs
data_raw_obs <-
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


# Estimate the number of sampling days per site (each day 3 surveys)

sampling_days <- data_raw_obs %>% group_by(site) %>% count() %>%
  mutate(days=n/3) %>% select(site,days) %>% rename(site_id=site) %>%
  mutate(total_area = days*3*40,total_time = days*3*40*45/60) #See explanation below

data.site <- data.site %>% merge(sampling_days,by="site_id")

data_raw_gather <- data_raw_obs %>% gather(-site,key = "Organism_ID", value = 'Abundance', !contains("site"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# Rader et al (2016) referred to the following methods (in 10.1111/j.1461-0248.2007.01110.x):
# We measured pollinator visitation rate to flowers
# during 45-s scans of flowers at 40 equally spaced points
# along each transect. We observed visits to as many flowers
# as we could view simultaneously within an approximately
# one by one metre area. Visitation rate data are therefore
# measured as bee visits per flower per time.
# We censused each transect three times per day and visited each farm on
# 2 days.

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0) %>% rename(site_id=site)

data_raw_gather <- data_raw_gather %>% left_join(sampling_days,"site_id")

insect_sampling <- tibble(
  study_id = "Rachael_Winfree_Citrullus_lanatus_USA_2011",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "transect observations",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = 450,
  total_sampled_time = data_raw_gather$total_time,
  total_sampled_flowers = NA,
  Description = "Pollinators were observed for a total of 30 minutes along a 50-m transect of crop row (within each transect, 40 equally spaced points were scanned for 45-s each).  Each transect was censused 3 times per day.  Sites were visited only once per year in 2008, but 3 times per year in 2010-2012.  Visits were divided by the number of flowers to calculate visitation rates."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Rachael_Winfree_Citrullus_lanatus_USA_2011.csv")
#setwd(dir_ini)

#######################################
# VISITATION RATE
#######################################

# Add site observations

data_raw_gather <-  data_raw_gather %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


visit_aux <- data_raw_gather %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>%
  spread(key=Guild, value=n)

names(visit_aux)

# There are ""bumblebees"      "honeybees"   "lepidoptera"     "other_flies"
# "other_wild_bees" "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(beetles=0,
                                  non_bee_hymenoptera=0,other=0,humbleflies=0,total=0)
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
  total_sampled_area = 450,
  total_sampled_time = data.site$total_time,
  visitation_rate_units = "visits per flower per 45-s observation period",
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

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Rachael_Winfree_Citrullus_lanatus_USA_2011.csv")
#setwd(dir_ini)

