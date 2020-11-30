# load libraries
library(tidyverse)
library("iNEXT")
library(parzer)
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Processing_files/Datasets_Processing/Finbarr_G_Horgan_several_crops_Philippines_2015/Crop pollinators.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(site_id,sampling_year) %>% count()

data.site %>% group_by(crop,sampling_year) %>% count()

data.site %>% group_by(crop,site_id) %>% count() %>% filter(n>1)

# Fix crop names

data.site$crop[data.site$crop=="Ampalaya - Bitter melon (Momordica charantia)"] <- "Momordica charantia"
data.site$crop[data.site$crop=="Cucumber (Cucumis sativus)"] <- "Cucumis sativus"
data.site$crop[data.site$crop=="Okra (Abelmoschus esculentus)"] <- "Abelmoschus esculentus"
data.site$crop[data.site$crop=="Patola - Luffa gourd (Luffa acutangula)"] <- "Luffa acutangula"
data.site$crop[data.site$crop=="Upo - Bottle gourd (Lagenaria siceraria)"] <- "Lagenaria siceraria"

# New study ID
data.site <- data.site %>% mutate(new_study_id=
                                    paste0("Finbarr_G_Horgan_",crop,"_Philippines_2015"))

data.site$new_study_id <- str_replace(data.site$new_study_id," ","_")

# Create new site_id's

# New study ID
data.site <- data.site %>% mutate(new_site_id=study_id) %>%
  separate(new_site_id,c("site","crops_informal","season"),"_") %>%
  mutate(new_site_id=paste0(site,"_",season))

labeling <- data.site %>% select(study_id,site_id,new_study_id,new_site_id)

# Rename management
data.site$management <- "conventional"


# Fix latitude and longitude
data.site$latitude <- parse_lat(data.site$latitude)
data.site$longitude <- parse_lon(data.site$longitude)



###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Processing_files/Datasets_Processing/Finbarr_G_Horgan_several_crops_Philippines_2015/Crop pollinators.xlsx", sheet = "insect_sampling")

# New study and sites ID

#Fix labels

insect_sampling$site_id[insect_sampling$site_id=="Bukidnon1"] <- "Bukidnon 1"
insect_sampling$site_id[insect_sampling$site_id=="Bukidnon2"] <- "Bukidnon 2"

# Add new labels

insect_sampling <- insect_sampling %>%
  left_join(labeling,by=c("study_id","site_id"))

# Fix guilds

insect_sampling %>% group_by(guild) %>% count()
insect_sampling$guild[insect_sampling$guild=="honeybee"] <-"honeybees"
insect_sampling$guild[insect_sampling$guild=="moths"] <-"lepidoptera"
insect_sampling$guild[insect_sampling$guild=="butterflies"] <-"lepidoptera"
insect_sampling$guild[insect_sampling$guild=="other wild bees"] <-"other_wild_bees"
insect_sampling %>% group_by(guild) %>% count()

#insect_sampling <- insect_sampling %>% filter(abundance>0)

insect_sampling_new <- tibble(
  study_id=insect_sampling$new_study_id,
  site_id=insect_sampling$new_site_id,
  pollinator=insect_sampling$pollinator,
  guild=insect_sampling$guild,
  sampling_method=insect_sampling$sampling_method,
  abundance=insect_sampling$abundance,
  total_sampled_area=insect_sampling$total_sampled_area,
  total_sampled_time=insect_sampling$total_sampled_time,
  total_sampled_flowers=insect_sampling$total_sampled_flowers,
  Description=insect_sampling$`Description_(fee_text)`
)

# Save new insect sampling templates
insect_sampling_new %>% group_by(study_id) %>% count()

insect_sampling_Abelmoschus <- insect_sampling_new %>%
  filter(study_id=="Finbarr_G_Horgan_Abelmoschus_esculentus_Philippines_2015")
insect_sampling_Cucumis <- insect_sampling_new %>%
  filter(study_id=="Finbarr_G_Horgan_Cucumis_sativus_Philippines_2015")
insect_sampling_Lagenaria <- insect_sampling_new %>%
  filter(study_id=="Finbarr_G_Horgan_Lagenaria_siceraria_Philippines_2015")
insect_sampling_Luffa <- insect_sampling_new %>%
  filter(study_id=="Finbarr_G_Horgan_Luffa_acutangula_Philippines_2015")
insect_sampling_Momordica <- insect_sampling_new %>%
  filter(study_id=="Finbarr_G_Horgan_Momordica_charantia_Philippines_2015")

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_Abelmoschus,
          "Processing_files/Datasets_storage/insect_sampling_Finbarr_G_Horgan_Abelmoschus_esculentus_Philippines_2015.csv")
write_csv(insect_sampling_Cucumis,
          "Processing_files/Datasets_storage/insect_sampling_Finbarr_G_Horgan_Cucumis_sativus_Philippines_2015.csv")
write_csv(insect_sampling_Lagenaria,
          "Processing_files/Datasets_storage/insect_sampling_Finbarr_G_Horgan_Lagenaria_siceraria_Philippines_2015.csv")
write_csv(insect_sampling_Luffa,
          "Processing_files/Datasets_storage/insect_sampling_Finbarr_G_Horgan_Luffa_acutangula_Philippines_2015.csv")
write_csv(insect_sampling_Momordica,
          "Processing_files/Datasets_storage/insect_sampling_Finbarr_G_Horgan_Momordica_charantia_Philippines_2015.csv")


#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- insect_sampling_new %>% select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,
                                          syrphids=0,
                                          humbleflies=0,
                                          other_flies=0,
                                          beetles=0,
                                          non_bee_hymenoptera=0,
                                          other=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

abundance_aux <- abundance_aux %>%
  rename(new_study_id=study_id,new_site_id=site_id)

data.site <- data.site %>% left_join(abundance_aux, by = c("new_study_id","new_site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling_new %>%
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

  if (sum(x)>0){
    chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
    abundace_field$r_obser[i] <-  chao$Observed
    abundace_field$r_chao[i] <-  chao$Estimator
  }else{
    abundace_field$r_obser[i] <-  0
    abundace_field$r_chao[i] <-  0.0
  }

}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="Bees, including honeybees; butterflies and diurnal moths")

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

richness_aux <- richness_aux %>%
  rename(new_study_id=study_id,new_site_id=site_id)

data.site <- data.site %>% left_join(richness_aux, by = c("new_study_id","new_site_id"))


##################
# VISITATION RATES
##################
insect_sampling_new$total_sampled_flowers <- as.numeric(insect_sampling_new$total_sampled_flowers)
sampling <- insect_sampling_new %>% group_by(study_id,site_id) %>% summarise(flowers=mean(total_sampled_flowers,na.rm = T),
                                                                         area=mean(total_sampled_area),
                                                                         time=mean(total_sampled_time)) %>%
  rename(new_study_id=study_id,new_site_id=site_id)

visit_aux <- abundance_aux %>% left_join(sampling,by=c("new_study_id","new_site_id")) %>%
  mutate(
    vist_beetles = 60*beetles/time,
    vist_bumblebees = 60*bumblebees/time,
    vist_honeybees = 60*honeybees/time,
    vist_humbleflies = 60*humbleflies/time,
    vist_lepidoptera = 60*lepidoptera/time,
    vist_non_bee_hymenoptera = 60*non_bee_hymenoptera/time,
    vist_other_flies = 60*other_flies/time,
    vist_other_wild_bees = 60*other_wild_bees/time,
    vist_others = 60*other/time,
    vist_syrphids =  60*syrphids/time,
    vist_total = vist_beetles + vist_bumblebees +
      vist_honeybees + vist_humbleflies +
      vist_lepidoptera + vist_non_bee_hymenoptera +
      vist_other_flies + vist_other_wild_bees + vist_others + vist_syrphids
  ) %>%
  select(new_study_id,new_site_id,vist_beetles,vist_bumblebees,vist_honeybees, vist_humbleflies,
           vist_lepidoptera,vist_non_bee_hymenoptera,vist_other_flies,
         vist_other_wild_bees,vist_others,vist_syrphids,vist_total
         )

data.site <- data.site %>% left_join(visit_aux,by=c("new_study_id","new_site_id"))
data.site <- data.site %>% left_join(sampling,by=c("new_study_id","new_site_id"))
###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = data.site$new_study_id,
  site_id = data.site$new_site_id,
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
  ab_others = data.site$other,
  total_sampled_area = data.site$area,
  total_sampled_time = data.site$time,
  visitation_rate_units = NA,#"visits per hour",
  visitation_rate = NA,#data.site$vist_total,
  visit_honeybee = NA,#data.site$vist_honeybees,
  visit_bombus = NA,#data.site$vist_bumblebees,
  visit_wildbees = NA,#data.site$vist_other_wild_bees,
  visit_syrphids = NA,#data.site$vist_syrphids,
  visit_humbleflies = NA,#data.site$vist_humbleflies,
  visit_other_flies = NA,#data.site$vist_other_flies,
  visit_beetles = NA,#data.site$vist_beetles,
  visit_lepidoptera = NA,#data.site$vist_lepidoptera,
  visit_nonbee_hymenoptera = NA,#data.site$vist_non_bee_hymenoptera,
  visit_others = NA,#data.site$vist_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)

# Save new field_level_data templates

field_level_data_Abelmoschus <- field_level_data %>%
  filter(study_id=="Finbarr_G_Horgan_Abelmoschus_esculentus_Philippines_2015")
field_level_data_Cucumis <- field_level_data %>%
  filter(study_id=="Finbarr_G_Horgan_Cucumis_sativus_Philippines_2015")
field_level_data_Lagenaria <- field_level_data %>%
  filter(study_id=="Finbarr_G_Horgan_Lagenaria_siceraria_Philippines_2015")
field_level_data_Luffa <- field_level_data %>%
  filter(study_id=="Finbarr_G_Horgan_Luffa_acutangula_Philippines_2015")
field_level_data_Momordica <- field_level_data %>%
  filter(study_id=="Finbarr_G_Horgan_Momordica_charantia_Philippines_2015")

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_Abelmoschus,
          "Processing_files/Datasets_storage/field_level_data_Finbarr_G_Horgan_Abelmoschus_esculentus_Philippines_2015.csv")
write_csv(field_level_data_Cucumis,
          "Processing_files/Datasets_storage/field_level_data_Finbarr_G_Horgan_Cucumis_sativus_Philippines_2015.csv")
write_csv(field_level_data_Lagenaria,
          "Processing_files/Datasets_storage/field_level_data_Finbarr_G_Horgan_Lagenaria_siceraria_Philippines_2015.csv")
write_csv(field_level_data_Luffa,
          "Processing_files/Datasets_storage/field_level_data_Finbarr_G_Horgan_Luffa_acutangula_Philippines_2015.csv")
write_csv(field_level_data_Momordica,
          "Processing_files/Datasets_storage/field_level_data_Finbarr_G_Horgan_Momordica_charantia_Philippines_2015.csv")

#setwd(dir_ini)
