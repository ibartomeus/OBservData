
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")


dir_ini <- getwd()

##########################
#Data: Hipolito_2005
##########################

data_raw <- read_csv("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/Hipolito_2005.csv")

# Remove columns full of NA's
data_raw_without_NAs <-
  data_raw[,colSums(is.na(data_raw))<nrow(data_raw)]


############################
# FIELD INFORMATION
############################

data.site_aux <- tibble(
  study_id = "Juliana_Hipólito_Mangifera_indica_Brazil_2005",
  site_id = data_raw_without_NAs$cultivar_other_label,
  crop = "Mangifera indica",
  variety = "Tommy Atkins",
  management = data_raw_without_NAs$land_management,
  country = "Brazil",
  latitude = NA,
  longitude = NA,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = 7,
  sampling_end_month = 7,
  sampling_year = data_raw_without_NAs$Year_of_study,
  field_size = NA,
  yield=NA,
  yield_units=NA,
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

library(parzer)

latitude <- c("S9° 27' 32","S9° 27' 32","S9° 28' 81","S9° 27' 08","S9° 30' 53")
longitude <- c("W40° 27' 45","W40° 27' 45","W40° 26' 06","W40° 22' 50","W40?° 20' 12")

data.site_aux$latitude <- parse_lat(latitude)
data.site_aux$longitude <- parse_lon(longitude)

########
#YIELD

data.site_aux$yield <- c(10,NA,NA,3,2.8)
data.site_aux$yield_units <- "Fruit set (%)"

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

data.site$Publication <- "10.4257/oeco.2010.1401.09"
data.site$Credit  <- "Juliana Hipólito de Sousa, Camila Magalhaes Pigozzo & Blandina Felipe Viana"
data.site$Email_contact <- "jhdsousa@yahoo.com"

###########################
# SAMPLING DATA
###########################

new_data_raw <- readxl::read_excel("Processing_files/Datasets_Processing/RADER 2016 DATABASE/Individual CSV/Mago_data_Juliana_Hipólito_Mangifera_indica_Brazil_2005.xls",skip = 2)

new_data_raw <- new_data_raw %>% rename(site_id=site)

which(names(new_data_raw)=="Apis mellifera")
which(names(new_data_raw)=="Diptera")

new_data_raw_obs <- new_data_raw %>%
  select(site_id, names(new_data_raw[26:70]))


data_raw_gather <- new_data_raw_obs %>%
  gather(-site_id,key = "Organism_ID", value = 'Abundance', !contains("site_id"))
data_raw_gather$Family <- as.character(NA)

#Add guild via guild list

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data_raw_gather <- data_raw_gather %>% left_join(gild_list,by=c("Organism_ID","Family"))

#Check NA's in guild
x <- data_raw_gather %>% filter(is.na(Guild))

data_raw_gather$Guild[grepl("Megachilidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Halictidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Trigonini",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
data_raw_gather$Guild[grepl("Coleoptera",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "beetles"
data_raw_gather$Guild[grepl("Vespidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
data_raw_gather$Guild[grepl("Pompilidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
data_raw_gather$Guild[grepl("Polistes",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
data_raw_gather$Guild[grepl("Culicidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Empididae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Muscidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Stratiomyoidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "syrphids"
data_raw_gather$Guild[grepl("Syrphus",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "syrphids"
data_raw_gather$Guild[grepl("Salpinigaster",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "syrphids"
data_raw_gather$Guild[grepl("Tachinidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Tipulidaesp",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Chironomidae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Diptera",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"
data_raw_gather$Guild[grepl("Calliphoridae",data_raw_gather$Organism_ID,ignore.case = TRUE)] <- "other_flies"

data_raw_gather %>% filter(is.na(Guild))

#######################
# INSECT SAMPLING
#######################

# Para a amostragem dos visitantes florais, foram selecionadas duas ?rvores,
# considerando a sua localiza??o central no plantio, intensidade e uniformidade da
# flora??o na copa. As coletas foram realizadas durante 24 horas em intervalos de 1
# hora com redes entomol?gicas.

# Remove entries with zero abundance
data_raw_gather <- data_raw_gather %>% filter(Abundance>0,!is.na(Abundance))

#Field 1 has been surveyed twice
total_time <- tibble(site_id=data.site$site_id,total_time=c(2*24*60,24*60,24*60,24*60))
data_raw_gather <- data_raw_gather %>% left_join(total_time,by="site_id")

insect_sampling <- tibble(
  study_id = "Juliana_Hipólito_Mangifera_indica_Brazil_2005",
  site_id = data_raw_gather$site_id,
  pollinator = data_raw_gather$Organism_ID,
  guild = data_raw_gather$Guild,
  sampling_method = "insect collection",
  abundance = data_raw_gather$Abundance,
  total_sampled_area = NA,
  total_sampled_time = data_raw_gather$total_time,
  total_sampled_flowers = NA,
  Description = "2 trees were selected in each field and insects were collected at 1 hour intervals over 24 hours"
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Juliana_Hipólito_Mangifera_indica_Brazil_2005.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_raw_gather <-  data_raw_gather %>%
  group_by(site_id,Organism_ID,Family,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_raw_gather %>%
  group_by(site_id,Guild) %>% count(wt=Abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are     "beetles"  "lepidoptera" "non_bee_hymenoptera"
# "syrphids" other_flies honeybees,other_wild_beea

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(bumblebees=0,other=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_raw_gather %>% ungroup %>%
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

# Load our estimation for taxonomic resolution

percentage_species_morphos <- 44/104

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

######################################################
# ESTIMATING VISITATION RATE
######################################################

visit_aux <- abundance_aux %>% left_join(total_time, by="site_id") %>% mutate(
  visitation_rate = 60*total/total_time/2,
  visit_honeybee = 60*honeybees/total_time/2,
  visit_bombus = 60*bumblebees/total_time/2,
  visit_wildbees = 60*other_wild_bees/total_time/2,
  visit_syrphids = 60*syrphids/total_time/2,
  visit_humbleflies = 60*humbleflies/total_time/2,
  visit_other_flies = 60*other_flies/total_time/2,
  visit_beetles = 60*beetles/total_time/2,
  visit_lepidoptera = 60*lepidoptera/total_time/2,
  visit_nonbee_hymenoptera = 60*non_bee_hymenoptera/total_time/2,
  visit_others = 60*other/total_time/2,
  visitation_rate_units="visits per tree and hour"
) %>% select(
  site_id, visitation_rate,visit_honeybee,visit_bombus,visit_wildbees,  visit_syrphids,
  visit_humbleflies,  visit_other_flies,  visit_beetles,  visit_lepidoptera,
  visit_nonbee_hymenoptera,  visit_others,  visitation_rate_units
)

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
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
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
  total_sampled_time = total_time$total_time,
  visitation_rate_units = data.site$visitation_rate_units,
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

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Juliana_Hipólito_Mangifera_indica_Brazil_2005.csv")
#setwd(dir_ini)

