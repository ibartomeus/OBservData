
library(tidyverse)
library(openxlsx)
library("iNEXT")

dir_ini <- getwd()

##########################
#Data: Bryony Willcox
##########################

# COMMENTS: The records of DIPTERA MORPHOS were assigned to "other_flies" 

#######################
# Insect sampling
#######################

#Load datasets: insect_sampling
data.insect <- read_csv("Copy of insect_sampling_Bryony_Willcox_several_crops_Australia_several_years_BKW.csv")

#######################
# Field level
#######################
#Load datasets: field_data
data.site <- read_csv("field_level_data_Bryony_Willcox_several_crops_Australia_several_years_BKW.csv")

row_avo_5_2016 <- data.site[4,]
row_avo_5_2017 <- data.site[4,]

# Update new records entries
row_avo_5_2016$study_id <- "Bryony_Willcox_Persea_americana_Australia_2016"
row_avo_5_2016$sampling_year <- 2016
row_avo_5_2016$yield <- NA
row_avo_5_2016[,16:58] <- NA


row_avo_5_2017$study_id <- "Bryony_Willcox_Persea_americana_Australia_2017"
row_avo_5_2017$sampling_start_month <- 8
row_avo_5_2017$sampling_year <- 2017
row_avo_5_2017$yield <- NA
row_avo_5_2017[,16:58] <- NA

data.site_new <- bind_rows(data.site,row_avo_5_2016,row_avo_5_2017) %>%arrange(sampling_year,site_id)


#####################################################
# FUNCTION TO EXTRACT ABUNDANCES AND RICHNESS, and RENAME STUDIES
#####################################################

extrac_abundance_richness <- function(row_avo_5_2016,data.site_new){

data.site_i <- data.site_new %>% filter(study_id==row_avo_5_2016$study_id[1],
                                        site_id==row_avo_5_2016$site_id[1],
                                    crop==row_avo_5_2016$crop[1],
                                    country==row_avo_5_2016$country[1],
                                    sampling_year==row_avo_5_2016$sampling_year[1])

data.insect_i <- data.insect %>% filter(study_id==row_avo_5_2016$study_id[1])

# Extract abundance

abundance_aux <- data.insect_i %>% group_by(site_id,guild) %>% 
  summarise(total_guild=sum(abundance)) %>% spread(guild,total_guild)
  
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

# Add missing guilds
guilds <- c("honeybees", "bumblebees", "other_wild_bees", "syrphids", "humbleflies",
            "other_flies", "beetles", "non_bee_hymenoptera", "lepidoptera", "others")

for (j in 1:length(guilds)){
  
  if (!guilds[j] %in% names(abundance_aux)){
    abundance_aux$aux <- 0
    names(abundance_aux)[names(abundance_aux)=="aux"] <- guilds[j]
  }
  
}


abundance_aux <- abundance_aux %>%
  rename(abundance = total, ab_bombus = bumblebees,
         ab_honeybee = honeybees, ab_wildbees = other_wild_bees,
         ab_syrphids = syrphids,	ab_humbleflies = humbleflies,	
         ab_other_flies = other_flies,	ab_beetles = beetles,
         ab_lepidoptera = lepidoptera,	ab_nonbee_hymenoptera = non_bee_hymenoptera,
         ab_others = others)

col_order <- c("site_id","ab_honeybee","ab_bombus","ab_wildbees","ab_humbleflies",
               "ab_beetles","ab_nonbee_hymenoptera", "ab_other_flies",
               "ab_syrphids", "ab_lepidoptera","ab_others","abundance")

abundance_aux <- abundance_aux[,col_order]

data.site_i_mod <- data.site_i %>% select(-abundance, -ab_bombus, -ab_honeybee, -ab_wildbees,
  -ab_syrphids,-ab_humbleflies, -ab_other_flies,-ab_beetles,-ab_lepidoptera,
  -ab_nonbee_hymenoptera,-ab_others)%>% 
  left_join(abundance_aux, by = "site_id")

# Extract richness

richness_aux <- data.insect_i %>% group_by(site_id,pollinator) %>% 
  summarise(total_pollinator=sum(abundance)) %>% spread(pollinator,total_pollinator)

richness_aux[is.na(richness_aux)] <- 0
richness_aux$r_obser <-  0
richness_aux$r_chao <-  0

for (j in 1:nrow(richness_aux)) {
  x <- as.numeric(richness_aux[j,2:(ncol(richness_aux)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  richness_aux$r_obser[j] <-  chao$Observed
  richness_aux$r_chao[j] <-  chao$Estimator 
}

#richness_aux <- richness_aux %>% select(site_id, pollinator_richness=r_chao) %>%
#  mutate(richness_estimator_method="Chao1")

abundance_non_bee <- rowSums(abundance_aux[,5:11],na.rm = TRUE)
if (sum(abundance_non_bee)>0){richness_restriction=NA}else{richness_restriction="Only bees"}


richness_aux <- richness_aux %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=richness_restriction)


data.site_i_mod <- data.site_i_mod %>% 
  select(-observed_pollinator_richness,-other_pollinator_richness,-other_richness_estimator_method,
  -richness_restriction)%>% 
  left_join(richness_aux, by = "site_id")


# Extract total sampled time and area

sampling_aux <- data.insect_i %>% select(site_id,total_sampled_area,total_sampled_time) %>%
  mutate(total_sampled_area=as.numeric(total_sampled_area)) %>%
  group_by(site_id) %>% summarise_all(mean,na.rm=T)

data.site_i_mod <- data.site_i_mod %>% 
  select(-total_sampled_area,-total_sampled_time)%>% 
  left_join(sampling_aux, by = "site_id")


###############################################################
# FIELD LEVEL DATA
###############################################################


field_level_data_i <- tibble(
  study_id=data.site_i_mod$study_id,
  site_id=data.site_i_mod$site_id,
  crop=data.site_i_mod$crop,
  variety=data.site_i_mod$variety,
  management=data.site_i_mod$management,
  country=data.site_i_mod$country,
  latitude=data.site_i_mod$latitude,
  longitude=data.site_i_mod$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=data.site_i_mod$sampling_start_month,
  sampling_end_month=data.site_i_mod$sampling_end_month,
  sampling_year=data.site_i_mod$sampling_year,
  field_size=data.site_i_mod$field_size,
  yield=data.site_i_mod$yield,
  yield_units=data.site_i_mod$yield_units,
  yield2=data.site_i_mod$yield2,
  yield2_units=data.site_i_mod$yield2_units,
  yield_treatments_no_pollinators=data.site_i_mod$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site_i_mod$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=data.site_i_mod$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site_i_mod$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site_i_mod$fruits_per_plant,
  fruit_weight=data.site_i_mod$fruit_weight,
  plant_density=data.site_i_mod$plant_density,
  seeds_per_fruit=data.site_i_mod$seeds_per_fruit,
  seeds_per_plant=data.site_i_mod$seeds_per_plant,
  seed_weight=data.site_i_mod$seed_weight,
  observed_pollinator_richness=data.site_i_mod$observed_pollinator_richness,
  other_pollinator_richness=data.site_i_mod$other_pollinator_richness,
  other_richness_estimator_method=data.site_i_mod$other_richness_estimator_method,
  richness_restriction=data.site_i_mod$richness_restriction,
  abundance=data.site_i_mod$abundance,
  ab_honeybee=data.site_i_mod$ab_honeybee,
  ab_bombus=data.site_i_mod$ab_bombus,
  ab_wildbees=data.site_i_mod$ab_wildbees,
  ab_syrphids=data.site_i_mod$ab_syrphids,
  ab_humbleflies=data.site_i_mod$ab_humbleflies,
  ab_other_flies=data.site_i_mod$ab_other_flies,
  ab_beetles=data.site_i_mod$ab_beetles,
  ab_lepidoptera=data.site_i_mod$ab_lepidoptera,
  ab_nonbee_hymenoptera=data.site_i_mod$ab_nonbee_hymenoptera,
  ab_others=data.site_i_mod$ab_others,
  total_sampled_area=data.site_i_mod$total_sampled_area,
  total_sampled_time=data.site_i_mod$total_sampled_time,
  visitation_rate_units=data.site_i_mod$visitation_rate_units,
  visitation_rate=data.site_i_mod$visitation_rate,
  visit_honeybee=data.site_i_mod$visit_honeybee,
  visit_bombus=data.site_i_mod$visit_bombus,
  visit_wildbees=data.site_i_mod$visit_wildbees,
  visit_syrphids=data.site_i_mod$visit_syrphids,
  visit_humbleflies=data.site_i_mod$visit_humbleflies,
  visit_other_flies=data.site_i_mod$visit_other_flies,
  visit_beetles=data.site_i_mod$visit_beetles,
  visit_lepidoptera=data.site_i_mod$visit_lepidoptera,
  visit_nonbee_hymenoptera=data.site_i_mod$visit_nonbee_hymenoptera,
  visit_others=data.site_i_mod$visit_others,
  Publication=data.site_i_mod$Publication,
  Credit=data.site_i_mod$Credit,
  Email_contact=data.site_i_mod$Email_contact
)


return(field_level_data_i)

}




field_level_data_avo_5_2016 <- extrac_abundance_richness(row_avo_5_2016,data.site_new)
field_level_data_avo_5_2017 <- extrac_abundance_richness(row_avo_5_2017,data.site_new)

##########################################################
# SAVE MODIFIED FIELD DATA
##########################################################

field_level_data <- bind_rows(data.site,
                              field_level_data_avo_5_2016,
                              field_level_data_avo_5_2017) %>%
  arrange(sampling_year,site_id)


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data,"field_level_data_Bryony_Willcox_several_crops_Australia_several_years.csv")
setwd(dir_ini)
