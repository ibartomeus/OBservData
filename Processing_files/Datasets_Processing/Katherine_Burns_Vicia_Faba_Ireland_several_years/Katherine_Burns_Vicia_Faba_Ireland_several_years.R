# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Processing_files/Datasets_Processing/Katherine_Burns_Vicia_Faba_Ireland_several_years/globalcrop.BEANS.BurnsStanley_REVISED.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(site_id,sampling_year) %>% count()



# New study ID
data.site$study_id[data.site$sampling_year==2018] <- "Katherine_LW_Burns_Vicia_faba_Ireland_2018"
data.site$study_id[data.site$sampling_year==2019] <- "Katherine_LW_Burns_Vicia_faba_Ireland_2019"


# Fix contact data

data.site$Credit <- "Katherine LW Burns & Dara A Stanley"
data.site$email <- "katherine.burns@ucdconnect.ie"
data.site$Publication <- NA



###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Processing_files/Datasets_Processing/Katherine_Burns_Vicia_Faba_Ireland_several_years/globalcrop.BEANS.BurnsStanley_REVISED.xlsx", sheet = "insect_sampling")

# New study ID

sites_2018 <- data.site$site_id[data.site$sampling_year==2018]

insect_sampling$study_id[insect_sampling$site_id %in% sites_2018] <- "Katherine_LW_Burns_Vicia_faba_Ireland_2018"
insect_sampling$study_id[!insect_sampling$site_id %in% sites_2018] <- "Katherine_LW_Burns_Vicia_faba_Ireland_2019"

# Fix guilds

insect_sampling %>% group_by(guild) %>% count()

insect_sampling$guild[insect_sampling$guild=="other flies"] <-"other_flies"
insect_sampling %>% group_by(guild) %>% count()

# Rename columns

insect_sampling <- insect_sampling %>%
  rename(Description=`Description (Fee text)`)

# Separate years

insect_sampling_2018 <- insect_sampling %>% filter(study_id=="Katherine_LW_Burns_Vicia_faba_Ireland_2018")
insect_sampling_2019 <- insect_sampling %>% filter(study_id=="Katherine_LW_Burns_Vicia_faba_Ireland_2019")

# Save new insect sampling templates

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_2018, "Processing_files/Datasets_storage/insect_sampling_Katherine_LW_Burns_Vicia_faba_Ireland_2018.csv")
write_csv(insect_sampling_2019, "Processing_files/Datasets_storage/insect_sampling_Katherine_LW_Burns_Vicia_faba_Ireland_2019.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- insect_sampling %>% select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other_wild_bees=0,
                                          syrphids=0,
                                          humbleflies=0,
                                          beetles=0,
                                          non_bee_hymenoptera=0,
                                          lepidoptera=0,
                                          other=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling %>%
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


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))

##################
# SAMPLING
##################
insect_sampling$total_sampled_flowers <- as.numeric(insect_sampling$total_sampled_flowers)
sampling <- insect_sampling %>% group_by(study_id,site_id) %>% summarise(flowers=mean(total_sampled_flowers,na.rm = T),
                                                                         area=mean(total_sampled_area),
                                                                         time=mean(total_sampled_time))



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
  latitude = data.site$latitude,
  longitude = data.site$longitude,
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
  total_sampled_area = data.site$area,
  total_sampled_time = data.site$time,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = data.site$visit_honeybee,
  visit_bombus = data.site$visit_bombus,
  visit_wildbees = data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidotera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)


# Separate years

field_level_data_2018 <- field_level_data %>% filter(study_id=="Katherine_LW_Burns_Vicia_faba_Ireland_2018")
field_level_data_2019 <- field_level_data %>% filter(study_id=="Katherine_LW_Burns_Vicia_faba_Ireland_2019")

# Save new insect sampling templates

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2018, "Processing_files/Datasets_storage/field_level_data_Katherine_LW_Burns_Vicia_faba_Ireland_2018.csv")
write_csv(field_level_data_2019, "Processing_files/Datasets_storage/field_level_data_Katherine_LW_Burns_Vicia_faba_Ireland_2019.csv")
#setwd(dir_ini)
