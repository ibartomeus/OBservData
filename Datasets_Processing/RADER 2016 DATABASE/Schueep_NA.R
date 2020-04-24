
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(readxl)

dir_ini <- getwd()

##########################
#Data: Schueep_NA
##########################

data_raw <- read_csv("Individual CSV/Schueep_NA.csv")

# Remove columns full of NA's
data_raw_without_NAs <- 
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = paste0(data_raw_without_NAs$author,"_NA"),
  site_id = data_raw_without_NAs$sample_num,
  crop = "Prunus avium",
  variety = NA,
  management = data_raw_without_NAs$land_management,
  country = "Switzerland",
  latitude = NA,
  longitude = NA,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = 2011,
  field_size = NA,
  yield = 100*data_raw_without_NAs$fruitset,
  yield_units= "Fruit set (%): percentage of intact marked flowers (not damaged by herbivores) that developed into swollen green fruits approximately three weeks after pollination per site",
  yield2= NA,
  yield2_units= NA,
  yield_treatments_no_pollinators= NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2= NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant= NA,
  fruit_weight=  NA,
  plant_density= NA,
  seeds_per_fruit= NA,
  seeds_per_plant= NA,
  seed_weight= NA
)

data.site <- data.site_aux %>% 
  group_by(study_id,site_id,crop,variety,management,country,
           latitude,longitude,X_UTM,zone_UTM,sampling_end_month,sampling_year,yield_units) %>% 
  summarise_all(mean, na.rm = TRUE)

# Columns full of NAs return NaN: Set those Nan to NA
# is.nan doesn't actually have a method for data frames, unlike is.na
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

data.site[is.nan(data.site)] <- NA

############################################################

# Convert Latitude/Longitude from degrees min sec to decimal
# 
# chd = substr(data.site$latitude, 3, 3)[1]
# chm = substr(data.site$latitude, 6, 6)[1]
# chs = substr(data.site$latitude, 9, 10)[1]
# 
# cd = char2dms(data.site$latitude,chd=chd,chm=chm,chs=chs)
# data.site$latitude <- as.numeric(cd)
# 
# chd = substr(data.site$longitude, 3, 3)[1]
# chm = substr(data.site$longitude, 5, 5)[1]
# chs = substr(data.site$longitude, 8, 9)[1]
# 
# cd = char2dms(data.site$longitude,chd = chd,chm = chm,chs = chs)
# data.site$longitude <- as.numeric(cd)

#########################
# Adding credit, Publication and contact

data.site$Publication <- "10.1098/rspb.2013.2667"
data.site$Credit  <- "Christof Schuepp, Felix Herzog and Martin H. Entling"
data.site$Email_contact <- "entling@uni-landau.de"

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(sample_num,cultivar_other_label,round,row,observation_location, names(data_raw[30:ncol(data_raw)]))

# Remove NAs
data_raw_obs <- 
  data_raw_obs[,colSums(is.na(data_raw_obs))<nrow(data_raw_obs)]


data_raw_gather <- data_raw_obs %>% rename(site_id = sample_num)%>%
  gather(-site_id,key = "Organism_ID", value = 'Abundance', !contains("site_id"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################


# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0,!is.na(Abundance))


# Add papers supplementary info

datafield <- read_excel("Individual CSV/rspb-2013-2667-file007.xls",
                       sheet = "Pollinator visitation")

datafield <- as_tibble(datafield)
datafield2 <- datafield %>% rename(site_id=`Landscape ID`,Organism_ID=`Visitor group`)
datafield2$Organism_ID <- str_replace(datafield2$Organism_ID, " ", "_")
datafield2$Organism_ID <- str_replace(datafield2$Organism_ID, "cf", "")

datafield2$Organism_ID[datafield2$Organism_ID=="Andrena_ gravida"] <- "Andrena_gravida"
datafield2$Organism_ID[datafield2$Organism_ID=="Andrena_cf gravida"] <- "Andrena_gravida"
datafield2$Organism_ID[datafield2$Organism_ID=="Chalcidoidea"] <- "Chalcidoidea_sp"
datafield2$Organism_ID[datafield2$Organism_ID=="Coleoptera"] <- "Coleoptera_sp."
datafield2$Organism_ID[datafield2$Organism_ID=="Empididae"] <- "Empididae_sp."
datafield2$Organism_ID[datafield2$Organism_ID=="Formicidae"] <- "formicidae"
datafield2$Organism_ID[datafield2$Organism_ID=="Polistes_sp."] <- "Polistes_sp"
datafield2$Organism_ID[datafield2$Organism_ID=="Pompilidae"] <- "Pompilidae_cf"
datafield2$Organism_ID[datafield2$Organism_ID=="Syrphidae"] <- "syrphidae"


data_raw_gather <- data_raw_gather %>% left_join(datafield2,by=c("site_id","Organism_ID"))


insect_sampling <- tibble(
  study_id = "Schueep_NA",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "observations",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = 90,
  total_sampled_flowers = NA,
  Description = "A group of two to three flowers (rarely one or four) were filmed for 30 min at each site, on three different days during bloom, and resulting in recordings of approx. 225 flower-minutes per site"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Schueep_NA.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_raw_gather2 <-  data_raw_gather %>% select(-`Contact behaviour`,-`Duration (sec)`,-Daytime,
                                               -`Flowers per video`) %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_raw_gather2 %>% 
  group_by(site_id,Guild) %>% count(wt=Abundance) %>% 
  spread(key=Guild, value=n)

names(abundance_aux)

# There are     "beetles" "honeybees" "" "non_bee_hymenoptera"
# "syrphids" other_flies, other_wild_bees

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,lepidoptera=0,
  other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>%
  select(site_id,Organism_ID,Abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=Abundance)

abundace_field <- abundace_field %>% spread(key=Organism_ID,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}

richness_aux <- abundace_field %>% select(site_id, r_chao)
richness_aux <- richness_aux %>% rename(pollinator_richness=r_chao) %>%
  mutate(richness_estimator_method="Chao1")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

######################################################
# visitation rate
######################################################

visit_aux <- data_raw_gather %>% mutate(visit_flower=Abundance/`Flowers per video`/0.5/100) %>%
  select(site_id,visit_flower,Guild)%>%
  group_by(site_id,Guild) %>% count(wt=visit_flower) %>% 
  spread(key=Guild, value=n)


visit_aux <- visit_aux %>% mutate(bumblebees=0,lepidoptera=0,
                                          other=0,humbleflies=0,total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(2:ncol(visit_aux))])

visit_aux <- visit_aux %>% rename(visitation_rate = total,visit_honeybee = honeybees,
                     visit_bombus = bumblebees,visit_wildbees = other_wild_bees,
                     visit_syrphids = syrphids,visit_humbleflies = humbleflies,
                     visit_other_flies = other_flies, visit_beetles = beetles,
                     visit_lepidoptera = lepidoptera,
                     visit_nonbee_hymenoptera = non_bee_hymenoptera,visit_others = other)

data.site <- data.site %>% left_join(visit_aux, by = "site_id")


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
  yield_treatments_pollen_supplement=data.site$yield_treatments_no_pollinators,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  pollinator_richness = data.site$pollinator_richness,
  richness_estimator_method = data.site$richness_estimator_method,
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
  total_sampled_area = NA,
  total_sampled_time = 90,
  visitation_rate_units = "visits in 100 flowers during one hour",
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = data.site$visit_honeybee,
  visit_bombus = data.site$visit_bombus,
  visit_wildbees = data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Schueep_NA.csv")
setwd(dir_ini)

