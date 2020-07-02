# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Crop_pollination_database.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(site_id,sampling_year) %>% count()

# New study ID
data.site$study_id[data.site$sampling_year==2019] <- "Katrine_Hansen_Psidium_guajava_Thailand_2019"
data.site$study_id[data.site$sampling_year==2020] <- "Katrine_Hansen_Psidium_guajava_Thailand_2020"
 
# Rename management
data.site$management <- "conventional"

# Addapt latitude and longitude

data.site$latitude <- parzer::parse_lat(data.site$latitude)
data.site$longitude <- parzer::parse_lon(data.site$longitude)



###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Crop_pollination_database.xlsx", sheet = "insect_sampling")
insect_sampling <- insect_sampling[1:35,]

# New study ID
insect_sampling$study_id[insect_sampling$site_id %in% c(1,3,4)] <- "Katrine_Hansen_Psidium_guajava_Thailand_2019"
insect_sampling$study_id[!insect_sampling$site_id %in% c(1,3,4)] <- "Katrine_Hansen_Psidium_guajava_Thailand_2020"

# Fix area and time
insect_sampling$total_sampled_area <- 10
insect_sampling$total_sampled_time <- 200

#Exploring data
insect_sampling %>% group_by(pollinator) %>% count()
insect_sampling %>% group_by(guild) %>% count()

# Fix pollinator names

insect_sampling$pollinator[insect_sampling$pollinator=="Apis dorcata"] <- "Apis dorsata"
insect_sampling$pollinator[insect_sampling$pollinator=="Apis florana"] <- "Apis florea"

# Sanity check
insect_sampling %>% group_by(pollinator) %>% count()

#Fix guild names

insect_sampling$guild[insect_sampling$guild=="Honeybees"] <-"honeybees"
insect_sampling$guild[insect_sampling$guild=="non-bee hymenoptera"] <-"non_bee_hymenoptera"
insect_sampling$guild[insect_sampling$guild=="other flies"] <-"other_flies"
insect_sampling$guild[insect_sampling$guild=="Other wild bees"] <-"other_wild_bees"

# Sanity check
insect_sampling %>% group_by(guild) %>% count()


# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description <- "At each study site,1-2 flowers within 1 m^2 quadrat on ten different trees were observed. 15+5 minutes of observation per tree. Note: All study sites either had their own A. cerana hives or there would be hives in the surrounding area."


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Katrine_Hansen_Psidium_guajava_Thailand_several_years.csv")
setwd(dir_ini)

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

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,syrphids=0, humbleflies=0,
                                          lepidoptera=0,others=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

abundance_aux$site_id <- as.numeric(abundance_aux$site_id)

data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))

data.site %>% group_by(study_id,site_id) %>% count()
abundance_aux %>% group_by(study_id,site_id) %>% count()

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

richness_aux$site_id <- as.numeric(richness_aux$site_id)
data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))


##################
# VISITATION RATES
##################

flowers <- insect_sampling %>% group_by(study_id,site_id) %>% summarise(flowers=mean(total_sampled_flowers))
flowers$site_id <- as.numeric(flowers$site_id)

visit_aux <- abundance_aux %>% left_join(flowers,by=c("study_id","site_id")) %>%
  mutate(
    vist_beetles = 60*100*beetles/200/flowers,
    vist_bumblebees = 60*100*bumblebees/200/flowers,
    vist_honeybees = 60*100*honeybees/200/flowers,
    vist_humbleflies = 60*100*humbleflies/200/flowers,
    vist_lepidoptera = 60*100*lepidoptera/200/flowers,
    vist_non_bee_hymenoptera = 60*100*non_bee_hymenoptera/200/flowers,
    vist_other_flies = 60*100*other_flies/200/flowers,
    vist_other_wild_bees = 60*100*other_wild_bees/200/flowers,
    vist_others = 60*100*others/200/flowers,
    vist_syrphids =  60*100*syrphids/200/flowers,
    vist_total = vist_beetles + vist_bumblebees + 
      vist_honeybees + vist_humbleflies + 
      vist_lepidoptera + vist_non_bee_hymenoptera +
      vist_other_flies + vist_other_wild_bees + vist_others + vist_syrphids
  ) %>%
  select(study_id,site_id,vist_beetles,vist_bumblebees,vist_honeybees, vist_humbleflies, 
         vist_lepidoptera,vist_non_bee_hymenoptera,vist_other_flies,
         vist_other_wild_bees,vist_others,vist_syrphids,vist_total
  )

data.site <- data.site %>% left_join(visit_aux,by=c("study_id","site_id"))

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
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  visitation_rate_units = "visits per 100 flowers during 1 hour",
  visitation_rate = data.site$vist_total,
  visit_honeybee = data.site$vist_honeybees,
  visit_bombus = data.site$vist_bumblebees,
  visit_wildbees = data.site$vist_other_wild_bees,
  visit_syrphids = data.site$vist_syrphids,
  visit_humbleflies = data.site$vist_humbleflies,
  visit_other_flies = data.site$vist_other_flies,
  visit_beetles = data.site$vist_beetles,
  visit_lepidoptera = data.site$vist_lepidoptera,
  visit_nonbee_hymenoptera = data.site$vist_non_bee_hymenoptera,
  visit_others = data.site$vist_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Katrine_Hansen_Psidium_guajava_Thailand_several_years.csv")
setwd(dir_ini)
