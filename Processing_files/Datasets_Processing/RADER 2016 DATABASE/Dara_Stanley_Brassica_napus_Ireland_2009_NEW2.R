
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Dara_Stanley_Brassica_napus_Ireland_2010
##########################

data_raw <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/stanley_stout_2dataset_2009.csv")

# Remove columns full of NA's
data_raw_without_NAs <-
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = "Dara_Stanley_Brassica_napus_Ireland_2009",
  site_id = data_raw_without_NAs$site,
  crop = "Brassica napus",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "Ireland",
  latitude = data_raw_without_NAs$latitude,
  longitude = data_raw_without_NAs$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = 5,
  sampling_end_month = 8,
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

###########################################################
#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1111/1365-2664.12060"
data.site$Credit  <- "Dara A. Stanley and Jane C. Stout"
data.site$Email_contact <- "stanleyd@tcd.ie"

###########################
# SAMPLING DATA
###########################

insect_sampling_dara <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/insect_sampling_Dara_Stanley_Brassica_napus_Ireland_2009_update.csv")

insect_sampling_dara_aggregated <- insect_sampling_dara %>%
  group_by(study_id,site_id,pollinator,guild,
           total_sampled_area,total_sampled_time,total_sampled_flowers) %>%
  count(wt=abundance) %>% rename(abundance=n)

#######################
# INSECT SAMPLING
#######################

# Fields were surveyed for flower-visiting insects four
# times during the flowering period of May16 to June 1, 2009.
# Insect surveys were conducted in three 200 ? 2 m transects per
# field.


insect_sampling <- tibble(
  study_id = "Dara_Stanley_Brassica_napus_Ireland_2009",
  site_id = insect_sampling_dara_aggregated$site_id,
  pollinator = insect_sampling_dara_aggregated$pollinator,
  guild = insect_sampling_dara_aggregated$guild,
  sampling_method = "transects",
  abundance = insect_sampling_dara_aggregated$abundance,
  total_sampled_area = 4*3*100*2,
  total_sampled_time = 4*3*100/0.255/60, #0.255 is the approximated average velocity
  total_sampled_flowers = NA,
  Description = "Each field was visited three times and there were four 100 ? 2 m transects walked on each sampling visit (two on the edge and two on the centre), at a slow, steady pace (0.07-0.44 m/s)."
)




#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Dara_Stanley_Brassica_napus_Ireland_2009.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_raw_gather <-  insect_sampling %>%
  dplyr::select(site_id,pollinator,guild,abundance) %>%
  group_by(site_id,pollinator,guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_raw_gather %>%
  group_by(site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# There are "bumblebees"      "honeybees"       "lepidoptera"     "other_wild_bees"
# "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other_flies=0,beetles=0,non_bee_hymenoptera=0,
                                          other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>%
  select(site_id,pollinator,abundance)%>%
  group_by(site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

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
percentage_species_morphos <- (69-14)/69

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.74){
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
  total_sampled_area = 4*3*100*2,
  total_sampled_time = 4*3*100/0.255/60,
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

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Dara_Stanley_Brassica_napus_Ireland_2009.csv")
#setwd(dir_ini)

