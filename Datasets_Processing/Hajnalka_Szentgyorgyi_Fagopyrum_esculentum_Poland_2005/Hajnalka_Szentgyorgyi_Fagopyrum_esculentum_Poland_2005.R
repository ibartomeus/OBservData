# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.transects <- read_excel("05 BW data all OBServ.xls", sheet = "Transects") %>% 
  filter(is.na(Notes),!is.na(Species))
data.transects <- as_tibble(data.transects)

# Sampling time (after removing the missing data)

time.sampling <- data.transects %>%
  select(Field,Series,Transect) %>% unique() %>%
  group_by(Field,Series) %>% count() %>% mutate(time=30*n) #Each transect takes 30 min.

# Species abundance obtain from transect counts
data.transects_ag <- data.transects %>% 
  group_by(Field,Series,Species) %>% count(wt=`No ind.`) %>% filter(!is.na(Species))


# Extract month of sampling from date 

data.transects$month_of_sampling <- NA

for (i in 1:nrow(data.transects)){
  x <- as.character(data.transects$Date[i])
  month <- unlist(strsplit(x, ""))[5:6]
  data.transects$month_of_sampling[i] <- as.numeric(paste0(month[1],month[2]))
  
}



############################
# FIELD INFORMATION
############################

data.site_aux <- read_excel("05 BW data all OBServ.xls", sheet = "Resume_crop_yield")
data.site_aux <- as_tibble(data.site_aux)

# fruits_per_plant:	average number of fruits per plant [count per plant]
# fruit_weight:	average fruit weight [grams per fruit]
# plant_density:	number of crop plants per unit area of crop field [individuals per square meter]
# seeds_per_fruit:	average number of seeds per fruit [count per fruit]
# seeds_per_plant:	average number of seeds per plant or pod [count per plant]
# seed_weight:	average seed weight [grams per 100 seeds]

data.site_aux <- data.site_aux %>% rename(site_id=Field,plant_density=`no. plants/ha`,seeds_per_plant=`No. seeds/plant`,
                                  seeds_ha = `no. seeds`,yield = `yield in kg/ha`) %>%
  mutate(plant_density=plant_density/10000,seed_weight=1000*100*yield/seeds_ha)

data.site <- data.site_aux %>% select(site_id,plant_density,seeds_per_plant,yield) %>%
  group_by(site_id) %>% summarise_all(mean,rm.na=T)

# Fix yield values for	Kolonia Branewska and Wólka Ratajska
data.site$yield[c(4,6)] <- NA

# Include additional site info

data.site$study_id <-  "Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005"
data.site$crop <-  "Fagopyrum esculentum"
data.site$variety <-  NA
data.site$management <-  "conventional" #extracted from RADER
data.site$country <-  "Poland"
data.site$latitude <-  NA
data.site$longitude <-  NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <-  NA
data.site$sampling_end_month <-  NA
data.site$sampling_year <-  2005
data.site$field_size <-  NA
data.site$yield_units <-  "kg/ha"
data.site$yield2 <-  NA
data.site$yield2_units <-  NA
data.site$yield_treatments_no_pollinators <-  NA
data.site$yield_treatments_pollen_supplement <- NA
data.site$yield_treatments_no_pollinators2 <-  NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <-  NA
data.site$fruit_weight <-   NA
data.site$seeds_per_fruit <-  NA
data.site$seed_weight <-  NA
data.site$Publication <- "10.1016/j.agee.2009.05.001"
data.site$Credit  <- "Hajnalka Szentgyorgyi"
data.site$Email_contact <- "hajnalka.szentgyorgyi@gmail.com"

# Fix sampling months

sites <- unique(data.site$site_id)
sites == unique(data.transects$Field)

data.transects$Field[data.transects$Field=="Kolonia Branewka"] <- "Kolonia Branewska"

for (i in sites){
  
  data.site$sampling_start_month[data.site$site_id==i] <- 
    data.transects %>% filter(Field==i) %>% 
    select(month_of_sampling) %>% filter(!is.na(month_of_sampling)) %>% min()
  
  data.site$sampling_end_month[data.site$site_id==i] <- 
    data.transects %>% filter(Field==i) %>% 
    select(month_of_sampling) %>% filter(!is.na(month_of_sampling)) %>% max()
  
}



###########################
# SAMPLING DATA: TRANSECTS
###########################

data_raw_obs <- data.transects_ag %>% ungroup %>%  rename(Organism_ID=Species,round=Series)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[grepl("Andrena",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Anthophora",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Apidae",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Atylotus rusticus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Dasypoda",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Eristalis",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Evylaeus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Gymnosoma",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Haemmatopota plurails",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Helophillus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Hylaeus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Rhagio",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Scathophaga",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Specodes",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Systoechus ctenopterus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Tabanus bovine",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Talmerus atricapillus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild$Guild[grepl("Volucella pelluceris",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"

#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################

# Add sampling info

sampling <- time.sampling %>% rename(site_id=Field,round=Series) %>% 
  mutate(area=n*150*4) %>% select(site_id,round,time,area) %>% unique() %>% ungroup() %>%
  select(-round) %>% group_by(site_id) %>% summarise_all(sum)

data_obs_guild  <- data_obs_guild  %>% rename(site_id=Field,abundance=n) %>%
  left_join(sampling,by="site_id")

insect_sampling <- tibble(
  study_id = "Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "transects",
  abundance = data_obs_guild$abundance,
  total_sampled_area = data_obs_guild$area,
  total_sampled_time =  data_obs_guild$time,
  total_sampled_flowers = NA,
  Description = "In each site, two sets of pan traps were placed and a 150 m-long (4-m wide) standardized transect line was set (observer walked this line in 30 min). Four rounds of bee sampling using both methods were conducted during the main flowering period for each crop from May to August 2005")



###########################
# SAMPLING DATA: PANTRAPS
###########################

data_raw_obs_pa <- read_excel("05 BW data all OBServ.xls", sheet = "Pantraps") %>%
  filter(is.na(Uwagi)) %>% mutate(`hour of collection`=as.numeric(`hour of collection`),
                                  hour=`hour of collection`-`hour of exposure`) %>%
  
  select(Field,series,Species,`No. ind.`,hour) %>% 
  rename(Organism_ID=Species,round=series,abundance=`No. ind.`) %>%
  filter(!is.na(Organism_ID))

# fix hour for Wólka Ratajska
data_raw_obs_pa$hour[data_raw_obs_pa$Field=="Wólka Ratajska" & is.na(data_raw_obs_pa$hour) ] <- 0


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms_pa <- select(data_raw_obs_pa,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild_pa <- list_organisms_pa %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild_pa %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild_pa$Guild[grepl("Andrena",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Anthophora",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Apidae",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Atylotus rusticus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Dasypoda",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Eristalis",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild_pa$Guild[grepl("Evylaeus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Gymnosoma",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Haemmatopota plurails",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Helophillus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild_pa$Guild[grepl("Hylaeus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Rhagio",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Scathophaga",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Specodes",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Systoechus ctenopterus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Tabanus bovine",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Talmerus atricapillus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Volucella pelluceris",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "syrphids"

list_organisms_guild_pa$Guild[grepl("Bombus veteranus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
list_organisms_guild_pa$Guild[grepl("Chloromyia",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Chrysotus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Haematopora pluralis",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Myiatropa florea",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild_pa$Guild[grepl("Psithyrus silvestris",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
list_organisms_guild_pa$Guild[grepl("Psythirus bohemicus",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
list_organisms_guild_pa$Guild[grepl("Rhangio tringarius",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_flies"
list_organisms_guild_pa$Guild[grepl("Seladonia tumulorum",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild_pa$Guild[grepl("Tabornus bromius",list_organisms_guild_pa$Organism_ID,ignore.case = FALSE)] <- "NA"

#Sanity Checks
list_organisms_guild_pa %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild_pa <- data_raw_obs_pa %>% left_join(list_organisms_guild_pa, by = "Organism_ID")


#######################
# INSECT SAMPLING: PANTRAPS
#######################

# Add sampling info

sampling_pa <- data_raw_obs_pa %>% select(Field,round,hour) %>% 
  mutate(time=24*60+100*hour) %>% unique() %>% select(-round,-hour) %>%
  group_by(Field) %>% summarise_all(sum,rm.na=T) %>% mutate(area=NA) %>% rename(site_id=Field)

data_obs_guild_pa  <- data_obs_guild_pa  %>% rename(site_id=Field) %>%
  left_join(sampling_pa,by="site_id")

insect_sampling_pa <- tibble(
  study_id = "Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005",
  site_id = data_obs_guild_pa$site_id,
  pollinator = data_obs_guild_pa$Organism_ID,
  guild = data_obs_guild_pa$Guild,
  sampling_method = "pantraps",
  abundance = data_obs_guild_pa$abundance,
  total_sampled_area = data_obs_guild_pa$area,
  total_sampled_time =  data_obs_guild_pa$time,
  total_sampled_flowers = NA,
  Description = "In each site, two sets of pan traps were placed and a 150 m-long (4-m wide) standardized transect line was set (observer walked this line in 30 min). Four rounds of bee sampling using both methods were conducted during the main flowering period for each crop from May to August 2005")

insect_sampling_total <- bind_rows(insect_sampling,insect_sampling_pa)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_total, "insect_sampling_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- data_obs_guild %>% select(site_id,Guild,abundance) %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are     ""bumblebees"      "honeybees"       "other_flies"     "other_wild_bees" "syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,
                                          other=0,humbleflies=0,non_bee_hymenoptera=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])


abundance_aux$site_id[abundance_aux$site_id=="Kolonia Branewka"] <- "Kolonia Branewska"

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
  filter(!is.na(Guild)) %>%
  select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>% 
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

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
percentage_species_morphos <- .9

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

richness_aux$site_id[richness_aux$site_id=="Kolonia Branewka"] <- "Kolonia Branewska"
data.site <- data.site %>% left_join(richness_aux,by="site_id")

##################
# SAMPLING
##################
sampling$site_id[sampling$site_id=="Kolonia Branewka"] <- "Kolonia Branewska"
data.site <- data.site %>% left_join(sampling,by="site_id")


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
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
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
  total_sampled_area = data.site$area,
  total_sampled_time = data.site$time,
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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Hajnalka_Szentgyorgyi_Fagopyrum_esculentum_Poland_2005.csv")
setwd(dir_ini)
