# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(lubridate)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Processing_files/Datasets_Processing/Charlie_Nicholson_Vaccinium_corymbosum_USA_several_years/Nicholson_OBServ_data_contribution_3Jul2020.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(site_id,sampling_year) %>% count()

# New study ID
data.site$study_id <- paste0("Charlie_Nicholson_Vaccinium_corymbosum_USA_",data.site$sampling_year)


# Rename management
data.site %>% group_by(management) %>% count()
data.site$management[data.site$management=="Conventional"] <- "conventional"
data.site$management[data.site$management=="Organic"] <- "organic"

# Addapt crop name

data.site$crop <- "Vaccinium corymbosum"

# Addapt sampling months

data.site$sampling_start_month <- month(data.site$sampling_start_month)
data.site$sampling_end_month <- month(data.site$sampling_end_month)

# Addapt DOI

data.site$Publication <- "10.1016/j.agee.2018.10.018; 10.1016/j.agee.2017.08.030"

###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Processing_files/Datasets_Processing/Charlie_Nicholson_Vaccinium_corymbosum_USA_several_years/Nicholson_OBServ_data_contribution_3Jul2020.xlsx", sheet = "insect_sampling")

number_sampling_dates <- insect_sampling %>% select(site_id,Year,Date) %>%
  unique() %>% group_by(site_id,Year) %>% count()

mean(number_sampling_dates$n)

# New study ID
insect_sampling$study_id <- paste0("Charlie_Nicholson_Vaccinium_corymbosum_USA_",insect_sampling$Year)

# adding total amount of sampling dates

insect_sampling <- insect_sampling %>% left_join(number_sampling_dates,by=c("site_id","Year"))

# Fix area and time
insect_sampling <- insect_sampling %>% mutate(total_sampled_area = n*total_sampled_area)
insect_sampling <- insect_sampling %>% mutate(total_sampled_time = n*total_sampled_time)

#Exploring data
insect_sampling %>% group_by(pollinator) %>% count()
insect_sampling %>% group_by(guild) %>% count()


#Fix guild names
insect_sampling$guild[insect_sampling$guild=="other wild bees"] <-"other_wild_bees"

# Sanity check
insect_sampling %>% group_by(guild) %>% count()


# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description <- paste0("Date: ",insect_sampling$Date,". ",insect_sampling$n," sampling rounds (sampling days) per year, 2 aerial netting collections per round; 4 x 20 m transect (80 m^2) for 10 minutes per collection. Nota bene: Honeybees were not collected.")

# Modify method
insect_sampling$sampling_method <- "netting"


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling[,1:10], "Processing_files/Datasets_storage/insect_sampling_Charlie_Nicholson_Vaccinium_corymbosum_USA_several_years.csv")
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

abundance_aux <- abundance_aux %>% mutate(honeybees=0,syrphids=0, humbleflies=0,
                                          other_flies=0, beetles=0, non_bee_hymenoptera=0,
                                          lepidoptera=0,others=0,total=0)
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
  mutate(other_richness_estimator_method="Chao1",richness_restriction="bombus and other wild bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))


###############################
# SAMPLING EFFORT
################################

sampling_effort <- number_sampling_dates %>% mutate(total_sampled_area = n*160,
                                                    total_sampled_time = n*20) %>%
  rename(sampling_year=Year)

data.site <- data.site %>% left_join(sampling_effort,by=c("sampling_year","site_id"))

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
  ab_others = data.site$others,
  total_sampled_area = data.site$total_sampled_area.y,
  total_sampled_time = data.site$total_sampled_time.y,
  visitation_rate_units = "visits per m^2 and hour",#"flower visits/m^2/10 mins"
  visitation_rate = 6*data.site$visitation_rate,
  visit_honeybee = 6*data.site$visit_honeybee,
  visit_bombus = 6*data.site$visit_bombus,
  visit_wildbees = 6*data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = 6*data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Charlie_Nicholson_Vaccinium_corymbosum_USA_several_years.csv")
#setwd(dir_ini)
