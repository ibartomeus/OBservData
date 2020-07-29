
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
data.insect <- read.xlsx("crop_pollination_database_BKW.xlsx",
                         sheet = "insect_sampling", startRow = 1)
data.insect <- as_tibble(data.insect)

# Remove extra columns

data.insect <- data.insect[,c(1:10)]

# Fix units

data.insect$total_sampled_area <- data.insect %>% 
  separate(total_sampled_area,c("total_sampled_area","exponent"),"m") %>% 
  select(total_sampled_area) %>% pull()

# Fix Guilds

data.insect$guild[data.insect$guild=="honey bees"] <- "honeybees"
data.insect$guild[data.insect$guild=="wild bees"] <- "other_wild_bees"
data.insect$guild[data.insect$guild=="non-bee hymenoptera"] <- "non_bee_hymenoptera"

data.insect %>% group_by(guild) %>% count()

data.insect %>% select(pollinator,guild) %>% unique() %>% filter(guild=="flies")

data.insect$guild[grepl("Lucilia",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Sarcophaga",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Helina",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Lauxaniidae",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Syrphidae",data.insect$pollinator,ignore.case = FALSE)] <- "syrphids"
data.insect$guild[grepl("Chrysomya",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Eristalinus",data.insect$pollinator,ignore.case = FALSE)] <- "syrphids"
data.insect$guild[grepl("Bombylidae",data.insect$pollinator,ignore.case = FALSE)] <- "humbleflies"
data.insect$guild[grepl("Syritta",data.insect$pollinator,ignore.case = FALSE)] <- "syrphids"
data.insect$guild[grepl("Sarcophaga",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Tabanidae",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Meloidae",data.insect$pollinator,ignore.case = FALSE)] <- "beetles"
data.insect$guild[grepl("Calliphora augur",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Metallea",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Syrrita",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Sapromyza",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Dirioxa pornis",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Tachinid",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Diptera",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Stomorhina discolor",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Chlororhinia exempta",data.insect$pollinator,ignore.case = FALSE)] <- "other_flies"
data.insect$guild[grepl("Monolepta australis",data.insect$pollinator,ignore.case = FALSE)] <- "beetles"

#######################
# Field level
#######################
#Load datasets: field_data
data.site <- read.xlsx("crop_pollination_database_BKW.xlsx",
                       sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

# Fix management

data.site$management <- "conventional"

# Fix crop name
data.site$crop[data.site$crop=="Mangofera indica"] <- "Mangifera indica"

# Create a list of studies
studies.data <- data.site %>% group_by(study_id,crop,country,sampling_year) %>% count()
studies.data$crop_label <- str_replace(studies.data$crop," ","_")

#####################################################
# ADD ABUNDANCES AND RICHNESS, and RENAME STUDIES
#####################################################

for (i in 1:nrow(studies.data)){

new_study_name <- 
  paste("Bryony_Willcox",studies.data$crop_label[i],studies.data$country[i],studies.data$sampling_year[i],sep = "_")

data.site_i <- data.site %>% filter(study_id==studies.data$study_id[i],
                                    crop==studies.data$crop[i],
                                    country==studies.data$country[i],
                                    sampling_year==studies.data$sampling_year[i])

data.insect_i <- data.insect %>% filter(study_id==studies.data$study_id[i])

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


#####################
#INSECT SAMPLING
#####################

insect_sampling_visits_i <- tibble(
  study_id = new_study_name,
  site_id = data.insect_i$site_id,
  pollinator = data.insect_i$pollinator,
  guild = data.insect_i$guild,
  sampling_method = data.insect_i$sampling_method,
  abundance = data.insect_i$abundance,
  total_sampled_area = data.insect_i$total_sampled_area,
  total_sampled_time = data.insect_i$total_sampled_time,
  total_sampled_flowers = data.insect_i$total_sampled_flowers,
  Description = data.insect_i$`Description_(fee_text)`
)

if (i==1){
  insect_sampling_visits <- insect_sampling_visits_i
}else{
  insect_sampling_visits <- bind_rows(insect_sampling_visits,insect_sampling_visits_i)
}


###############################################################
# FIELD LEVEL DATA
###############################################################


field_level_data_i <- tibble(
  study_id=new_study_name,
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
  fruits_per_plant=data.site_i_mod$mean_fruits_per_plant,
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
  Email_contact=data.site_i_mod$email
)

if (i==1){
  field_level_data <- field_level_data_i
}else{
  field_level_data <- bind_rows(field_level_data,field_level_data_i)
}

}

##########################################################
# SAVE MODIFIED INSECT SAMPLING and FIELD DATA TABS
##########################################################

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_visits,"insect_sampling_Bryony_Willcox_several_crops_Australia_several_years.csv")
setwd(dir_ini)


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data,"field_level_data_Bryony_Willcox_several_crops_Australia_several_years.csv")
setwd(dir_ini)
