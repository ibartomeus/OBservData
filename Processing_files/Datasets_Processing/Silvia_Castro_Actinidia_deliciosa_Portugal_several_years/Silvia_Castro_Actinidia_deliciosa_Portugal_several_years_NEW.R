# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Processing_files/Datasets_Processing/Silvia_Castro_Actinidia_deliciosa_Portugal_several_years/Crop_pollination_database_I9Kiwi_2020-07-28.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(sampling_year) %>% count()

# New study ID
data.site$study_id[data.site$sampling_year==2018] <- "Silvia_Castro_Actinidia_deliciosa_Portugal_2018"
data.site$study_id[data.site$sampling_year==2019] <- "Silvia_Castro_Actinidia_deliciosa_Portugal_2019"

# Rename management
data.site$management <- "conventional"


# Fix latitude and longitude
data.site$latitude <- parse_lat(data.site$latitude)
data.site$longitude <- parse_lon(data.site$longitude)

#Fix yield A1 (2018)

data.site[1,13] <- "33.0"


###########################
# SAMPLING DATA
###########################


# New insect sampling templates

insect_sampling_2018 <- read_csv("Processing_files/Datasets_Processing/Silvia_Castro_Actinidia_deliciosa_Portugal_several_years/insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2018_CORRECTED.csv")

insect_sampling_2018 %>% group_by(guild) %>% count()

insect_sampling_2019 <- read_csv("Processing_files/Datasets_Processing/Silvia_Castro_Actinidia_deliciosa_Portugal_several_years/insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2019_CORRECTED_v2.csv")

insect_sampling_2019 %>% group_by(guild) %>% count()

# Fix guilds

insect_sampling_2019$guild[insect_sampling_2019$guild=="bombus"] <- "bumblebees"
insect_sampling_2019$guild[insect_sampling_2019$guild=="honeybee"] <- "honeybees"
insect_sampling_2019$guild[insect_sampling_2019$guild=="wildbees"] <- "other_wild_bees"
insect_sampling_2019$guild[insect_sampling_2019$guild=="nonbee_hymenoptera"] <- "non_bee_hymenoptera"
insect_sampling_2019$guild[insect_sampling_2019$guild=="others"] <- "other"

# Sanity check
insect_sampling_2019 %>% group_by(guild) %>% count()

# Filter NA
insect_sampling_2019 <- insect_sampling_2019 %>% filter(!is.na(guild))

# Sanity check
insect_sampling_2019 %>% group_by(guild) %>% count()

insect_sampling_2019$total_sampled_flowers <- as.numeric(insect_sampling_2019$total_sampled_flowers)
insect_sampling_2019$total_sampled_area <- as.numeric(insect_sampling_2019$total_sampled_area)


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_2018, "Processing_files/Datasets_storage/insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2018.csv")
write_csv(insect_sampling_2019, "Processing_files/Datasets_storage/insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2019.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

insect_sampling_2019$total_sampled_area <- as.numeric(insect_sampling_2019$total_sampled_area)
insect_sampling_2019$total_sampled_flowers <- as.numeric(insect_sampling_2019$total_sampled_flowers)

insect_sampling <- bind_rows(insect_sampling_2018,insect_sampling_2019)

abundance_aux <- insect_sampling %>% filter(!is.na(abundance),sampling_method=="census") %>%
  select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])


data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling %>% filter(!is.na(abundance),sampling_method=="census") %>%
  select(study_id, site_id,pollinator,abundance)%>%
  group_by(study_id,site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>%
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,3:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator
}

#############
# ESTIMATING PERCENTAGE OF SPECIES + MORPHOS

# There are no insect labels repeated in each field
insect_sampling %>% filter(sampling_method=="census") %>%
  group_by(study_id,site_id,pollinator) %>% count() %>%
  filter(n>1)

census_data <- insect_sampling %>%
  filter(sampling_method=="census") %>%
  select(study_id,pollinator) %>% unique()

# Commented for security reasons
# write_csv(census_data,"taxon_castro.csv")

taxon_kiwi <- read_csv("taxon_castro_kiwi.csv")
census_data[,1:2]==taxon_kiwi[,1:2]

insect_sampling_census_taxon <- insect_sampling %>%
  filter(sampling_method=="census") %>%
  left_join(taxon_kiwi,by=c("study_id","pollinator"))

insect_sampling_census_taxon %>%
  filter(study_id=="Silvia_Castro_Actinidia_deliciosa_Portugal_2018") %>%
  group_by(rank) %>% count()


percentage_species_morphos_2018 <- 15/18

insect_sampling_census_taxon %>%
  filter(study_id!="Silvia_Castro_Actinidia_deliciosa_Portugal_2018") %>%
  group_by(rank) %>% count()

percentage_species_morphos_2019 <- 123/(13+71+1+123)


richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)


if (percentage_species_morphos_2018 < 0.75){
  richness_aux[1:4,3:ncol(richness_aux)] <- NA
}

if (percentage_species_morphos_2019 < 0.75){
  richness_aux[5:nrow(richness_aux),3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))

##################
# VISITATION RATES
##################
insect_sampling$total_sampled_flowers <- as.numeric(insect_sampling$total_sampled_flowers)
sampling <- insect_sampling %>% filter(!is.na(abundance),sampling_method=="census") %>%
  group_by(study_id,site_id) %>%
  summarise(flowers=mean(total_sampled_flowers,na.rm = T),
            area=mean(total_sampled_area,na.rm = T),
            time=mean(total_sampled_time,na.rm = T))

# This calculation is wrong. Silvia provided an exact result that is based on interactions
# between plant and animals
#
# visit_aux <- abundance_aux %>% left_join(sampling,by=c("study_id","site_id")) %>%
#   mutate(
#     vist_beetles = 60*100*beetles/time/flowers,
#     vist_bumblebees = 60*100*bumblebees/time/flowers,
#     vist_honeybees = 60*100*honeybees/time/flowers,
#     vist_humbleflies = 60*100*humbleflies/time/flowers,
#     vist_lepidoptera = 60*100*lepidoptera/time/flowers,
#     vist_non_bee_hymenoptera = 60*100*non_bee_hymenoptera/time/flowers,
#     vist_other_flies = 60*100*other_flies/time/flowers,
#     vist_other_wild_bees = 60*100*other_wild_bees/time/flowers,
#     vist_others = 60*100*other/time/flowers,
#     vist_syrphids =  60*100*syrphids/time/flowers,
#     vist_total = vist_beetles + vist_bumblebees +
#       vist_honeybees + vist_humbleflies +
#       vist_lepidoptera + vist_non_bee_hymenoptera +
#       vist_other_flies + vist_other_wild_bees + vist_others + vist_syrphids
#   ) %>%
#   select(study_id,site_id,vist_beetles,vist_bumblebees,vist_honeybees, vist_humbleflies,
#            vist_lepidoptera,vist_non_bee_hymenoptera,vist_other_flies,
#          vist_other_wild_bees,vist_others,vist_syrphids,vist_total
#          )
#
# data.site <- data.site %>% left_join(visit_aux,by=c("study_id","site_id"))
data.site <- data.site %>% left_join(sampling,by=c("study_id","site_id"))

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
  latitude = NA,#data.site$latitude, #Anonimous
  longitude = NA,#data.site$longitude,#Anonimous
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$mean_fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness.y,
  other_pollinator_richness=data.site$other_pollinator_richness.y,
  other_richness_estimator_method=data.site$other_richness_estimator_method.y,
  richness_restriction = data.site$richness_restriction.y,
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
  total_sampled_time = data.site$time,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$visitation_rate,#data.site$vist_total,
  visit_honeybee = data.site$visit_honeybee, #data.site$vist_honeybees,
  visit_bombus = data.site$visit_bombus, #data.site$vist_bumblebees,
  visit_wildbees =data.site$visit_wildbees, #data.site$vist_other_wild_bees,
  visit_syrphids = data.site$visit_syrphids, #data.site$vist_syrphids,
  visit_humbleflies = data.site$visit_humbleflies, #data.site$vist_humbleflies,
  visit_other_flies = data.site$visit_other_flies, #data.site$vist_other_flies,
  visit_beetles = data.site$visit_beetles, #data.site$vist_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera, #data.site$vist_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,#data.site$vist_non_bee_hymenoptera,
  visit_others = data.site$visit_others,#data.site$vist_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)


field_level_data_2018 <- field_level_data %>% filter(sampling_year==2018)
field_level_data_2019 <- field_level_data %>% filter(sampling_year==2019)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2018, "Processing_files/Datasets_storage/field_level_data_Silvia_Castro_Actinidia_deliciosa_Portugal_2018.csv")
write_csv(field_level_data_2019, "Processing_files/Datasets_storage/field_level_data_Silvia_Castro_Actinidia_deliciosa_Portugal_2019.csv")
#setwd(dir_ini)

