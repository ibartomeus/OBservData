
library(tidyverse)
library(openxlsx)
library("iNEXT")

dir_ini <- getwd()

##########################
#Data: Nicolas J. Vereecken and his PhD student Timothy Weekers: vere01

#Load datasets: field_data
data.site <- read.xlsx("Processing_files/Datasets_Processing/Nicolas_J_Vereecken_several_crops_several_countries_several_years/Observ_TW_NEW2.xlsx",
                          sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

#Load datasets: insect_sampling
data.insect <- read.xlsx("Processing_files/Datasets_Processing/Nicolas_J_Vereecken_several_crops_several_countries_several_years/Observ_TW_NEW2.xlsx",
                         sheet = "insect_sampling", startRow = 1)
data.insect <- as_tibble(data.insect)

#Sanity checks sites:
# Guilds
data.insect %>% group_by(Guild) %>% count()
# Fix guilds

data.insect$Guild[data.insect$Guild=="honey bees"] <- "honeybees"
data.insect %>% group_by(Guild) %>% count()

#Sampling methods
data.insect %>% group_by(Sampling.method) %>% count()

#Sites
data.insect$site_id[!data.insect$site_id %in% data.site$site_id]
data.site$site_id[!data.site$site_id %in% data.insect$site_id]


#Fixing months' format
months_id <- c("March" = "3", "April" = "4", "May" = "5", "June" = "6", "July" = "7",
               "August" = "8")

data.site$sampling_end_month <- unname(months_id[data.site$sampling_end_month])
data.site$sampling_start_month <- unname(months_id[data.site$sampling_start_month])

#sanity check:
data.site$sampling_start_month %>% unique()
data.site$sampling_end_month %>% unique()

#Fixing total time format [hours -> mins]:

data.site$total_sampled_time <- 60*as.numeric(gsub("[^[:digit:]]", "", data.site$total_sampled_time))
data.insect$Total_sampled_time <- 60*as.numeric(gsub("[^[:digit:]]", "", data.insect$Total_sampled_time))

# Add email and credit
data.site$email <- "Nicolas.Vereecken@ulb.be, Timothy.weekers@ulb.be"
data.site$Credit <- "Nicolas J. Vereecken, Timothy Weekers, Nicolas Leclercq"

# Create a list of studies
studies.data <- data.site %>% group_by(study_id,crop,country,sampling_year) %>% count()


#####################################################
# ADD ABUNDANCES AND RICHNESS, and RENAME STUDIES
#####################################################

for (i in 1:nrow(studies.data)){

new_study_name <- paste(
  "Nicolas_J_Vereecken",str_replace_all(studies.data$crop[i]," ","_"),
  studies.data$country[i],studies.data$sampling_year[i],sep = "_")

data.site_i <- data.site %>% filter(study_id==studies.data$study_id[i],
                                    crop==studies.data$crop[i],
                                    country==studies.data$country[i],
                                    sampling_year==studies.data$sampling_year[i])

data.insect_i <- data.insect %>% filter(study_id==studies.data$study_id[i],
                                        site_id %in% data.site_i$site_id)

# Extract abundance

# Not every study contains bombus, honeybees or otherwildbees. We create those guilds
# by adding to data.insect_i three auxiliary rows with 0 abundances and the required guilds

data.insect_i_aux_honey <- data.insect_i[nrow(data.insect_i),] %>%
  mutate(Abundance=0,Guild="honeybees")
data.insect_i_aux_bombus <- data.insect_i[nrow(data.insect_i),] %>%
  mutate(Abundance=0,Guild="bumblebees")
data.insect_i_aux_wild <- data.insect_i[nrow(data.insect_i),] %>%
  mutate(Abundance=0,Guild="other wild bees")

data.insect_i_aux <- bind_rows(data.insect_i,data.insect_i_aux_honey,data.insect_i_aux_bombus,
                               data.insect_i_aux_wild)

abundance_aux <- data.insect_i_aux %>% group_by(site_id,Guild) %>%
  summarise(total_guild=sum(Abundance)) %>% spread(Guild,total_guild)

abundance_aux[is.na(abundance_aux)] <- 0

abundance_aux <- abundance_aux %>%
  mutate(total=bumblebees+honeybees+`other wild bees`) %>%
  rename(abundance = total, ab_bombus = bumblebees,
         ab_honeybee = honeybees, ab_wildbees = `other wild bees`)

data.site_i_mod <- data.site_i %>% select(-ab_bombus,-ab_honeybee,-ab_wildbees,-abundance)%>%
  left_join(abundance_aux, by = "site_id")

# Extract richness

richness_aux <- data.insect_i %>% group_by(site_id,Pollinator) %>%
  summarise(total_pollinator=sum(Abundance)) %>% spread(Pollinator,total_pollinator)

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

richness_aux <- richness_aux %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")


data.site_i_mod <- data.site_i_mod %>%
  select(-pollinator_richness,-richness_estimator_.Method)%>%
  left_join(richness_aux, by = "site_id")


#####################
#INSECT SAMPLING
#####################

insect_sampling_visits_i <- tibble(
  study_id = new_study_name,
  site_id = data.insect_i$site_id,
  pollinator = data.insect_i$Pollinator,
  guild = data.insect_i$Guild,
  sampling_method = data.insect_i$Sampling.method,
  abundance = data.insect_i$Abundance,
  total_sampled_area = data.insect_i$Total_sampled_area,
  total_sampled_time = data.insect_i$Total_sampled_time,
  total_sampled_flowers = data.insect_i$Total_sampled_flowers,
  Description = data.insect_i$`Description.(Fee.text)`
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
  field_size=data.site_i_mod$field.size,
  yield=data.site_i_mod$total_yield,
  yield_units="kg/ha",
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=data.site_i_mod$mean_fruits_per_plant,
  fruit_weight=data.site_i_mod$fruit_weight,
  plant_density=NA,
  seeds_per_fruit=data.site_i_mod$seeds_per_fruit,
  seeds_per_plant=data.site_i_mod$seeds_per_plant,
  seed_weight=data.site_i_mod$seed_weight,
  observed_pollinator_richness=data.site_i_mod$observed_pollinator_richness,
  other_pollinator_richness=data.site_i_mod$other_pollinator_richness,
  other_richness_estimator_method=data.site_i_mod$other_richness_estimator_method,
  richness_restriction="Only bees",
  #pollinator_richness=data.site_i_mod$pollinator_richness,
  #richness_estimator_method=data.site_i_mod$richness_estimator_method,
  abundance=data.site_i_mod$abundance,
  ab_honeybee=data.site_i_mod$ab_honeybee,
  ab_bombus=data.site_i_mod$ab_bombus,
  ab_wildbees=data.site_i_mod$ab_wildbees,
  ab_syrphids=data.site_i_mod$ab_syrphids,
  ab_humbleflies=NA,
  ab_other_flies=NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others=data.site_i_mod$ab_others,
  total_sampled_area=data.site_i_mod$total_sampled_area,
  total_sampled_time=data.site_i_mod$total_sampled_time,
  visitation_rate_units=NA,
  visitation_rate=data.site_i_mod$visitation_rate,
  visit_honeybee=data.site_i_mod$visit_honeybee,
  visit_bombus=data.site_i_mod$visit_bombus,
  visit_wildbees=data.site_i_mod$visit_wildbees,
  visit_syrphids=data.site_i_mod$visit_syrphids,
  visit_humbleflies=NA,
  visit_other_flies=NA,
  visit_beetles=NA,
  visit_lepidoptera=NA,
  visit_nonbee_hymenoptera=NA,
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

# Fix Management
field_level_data$management %>% unique()

field_level_data$management[field_level_data$management=="Conventional"] <- "conventional"
field_level_data$management[field_level_data$management=="Organic"] <- "organic"
field_level_data$management[field_level_data$management=="Non-organic"] <- "IPM"

############
# Sanity check
field_level_data$site_id %>% unique()
insect_sampling_visits$site_id %>% unique()

field_level_data$site_id[!field_level_data$site_id %in% insect_sampling_visits$site_id]
insect_sampling_visits$site_id[!insect_sampling_visits$site_id %in% field_level_data$site_id]
##########################################################
# SAVE MODIFIED INSECT SAMPLING and FIELD DATA TABS
##########################################################

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_visits,"Processing_files/Datasets_storage/insect_sampling_Nicolas_J_Vereecken_several_crops_several_countries_several_years.csv")
#setwd(dir_ini)


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data,"Processing_files/Datasets_storage/field_level_data_Nicolas_J_Vereecken_several_crops_several_countries_several_years.csv")
#setwd(dir_ini)
