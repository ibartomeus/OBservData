# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Crop_pollination_database_I9Kiwi_2020-07-28.xlsx", sheet = "field_level_data")
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



###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Crop_pollination_database_I9Kiwi_2020-07-28.xlsx", sheet = "insect_sampling")

# New study ID
insect_sampling$study_id[insect_sampling$`Description_(fee_text)` =="census from 2018"] <- "Silvia_Castro_Actinidia_deliciosa_Portugal_2018"
insect_sampling$study_id[insect_sampling$`Description_(fee_text)` !="census from 2018"] <- "Silvia_Castro_Actinidia_deliciosa_Portugal_2019"

#Fix site labels
insect_sampling$site_id[insect_sampling$site_id=="C"] <- "C2"
insect_sampling$site_id[insect_sampling$site_id=="B2"] <- "B"

#Fix guilds

insect_sampling %>% group_by(guild) %>% count()
insect_sampling$guild[insect_sampling$guild=="nonbee_hymenoptera"] <- "non_bee_hymenoptera"
insect_sampling$guild[insect_sampling$guild=="wildbees"] <- "other_wild_bees"
insect_sampling$guild[insect_sampling$guild=="bombus"] <- "bumblebees"
insect_sampling$guild[insect_sampling$guild=="honeybee"] <- "honeybees"
insect_sampling %>% group_by(guild) %>% count()

# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description[insect_sampling$sampling_method=="census"] <- insect_sampling$...11[insect_sampling$sampling_method=="census"]
insect_sampling$Description[insect_sampling$sampling_method!="census"] <- "3 sets of 3 colour plates each (within set, plates displayed in triangle 3m appart), sets displayed in transect in the middle of the orchard at 5, 35 and 65m from the field margin. Only pollinator groups were considered for the pan-traps."

# Fix records for total_sampled_area and total_sampled_flowers 
insect_sampling$total_sampled_area[insect_sampling$sampling_method=="pan-traps"] <- NA
insect_sampling$total_sampled_flowers[insect_sampling$sampling_method=="pan-traps"] <- NA


# Save new insect sampling templates

insect_sampling_2018 <- insect_sampling %>% 
  filter(study_id=="Silvia_Castro_Actinidia_deliciosa_Portugal_2018",
         !is.na(abundance), abundance>0) %>% select(-...11)

insect_sampling_2019 <- insect_sampling %>% 
  filter(study_id=="Silvia_Castro_Actinidia_deliciosa_Portugal_2019",
         !is.na(abundance), abundance>0) %>% select(-...11)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_2018, "insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2018.csv")
write_csv(insect_sampling_2019, "insect_sampling_Silvia_Castro_Actinidia_deliciosa_Portugal_2019.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

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


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 0.5

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
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

visit_aux <- abundance_aux %>% left_join(sampling,by=c("study_id","site_id")) %>%
  mutate(
    vist_beetles = 60*100*beetles/time/flowers,
    vist_bumblebees = 60*100*bumblebees/time/flowers,
    vist_honeybees = 60*100*honeybees/time/flowers,
    vist_humbleflies = 60*100*humbleflies/time/flowers,
    vist_lepidoptera = 60*100*lepidoptera/time/flowers,
    vist_non_bee_hymenoptera = 60*100*non_bee_hymenoptera/time/flowers,
    vist_other_flies = 60*100*other_flies/time/flowers,
    vist_other_wild_bees = 60*100*other_wild_bees/time/flowers,
    vist_others = 60*100*others/time/flowers,
    vist_syrphids =  60*100*syrphids/time/flowers,
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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2018, "field_level_data_Silvia_Castro_Actinidia_deliciosa_Portugal_2018.csv")
write_csv(field_level_data_2019, "field_level_data_Silvia_Castro_Actinidia_deliciosa_Portugal_2019.csv")
setwd(dir_ini)

