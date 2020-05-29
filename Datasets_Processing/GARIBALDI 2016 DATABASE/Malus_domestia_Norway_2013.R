# load libraries
library(tidyverse)
library("iNEXT")
#library(readxl)
library(openxlsx)
library(parzer)

dir_ini <- getwd()

science_raw <- read.delim("Database_Science.txt",sep = " ")

# Malus_domestia_Norway_2013

# Load data
data_raw <- read.xlsx("Norway/Norway_JÅ.xlsx",startRow = 1)

# Select data 
data_raw <- data_raw %>% filter(Crop_species!="Trifolium pratense")



###########################
# SAMPLING DATA
###########################

# NOTE: RICHNESS IN GARIBALDI'S TEMPLATE IS NOT OK. THE CORRECT ONE IS THAT IN
# SCIENCE SUPPL. MAT.

data_raw_obs_ini <- read_csv("Norway/apple_raw_div_2013.csv")

data_raw_obs_ini %>% group_by(Field,Plot.nr) %>% count()

# Gather abundance info

data_raw_obs <- data_raw_obs_ini %>% gather(Organism_ID,abundance,c(4:38)) %>%
  filter(abundance>0) %>% select(-Plot.nr,-Recording)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[grepl("B..",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
list_organisms_guild$Guild[grepl("Chrysotoxum",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Epistrophe",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Parasyrphus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Syrphus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Dasysyrphus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Platycheirus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Eristalis",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Leucozona.lucorum",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Empididae.sp.",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_flies"

list_organisms_guild$Guild[grepl("Melanogaster.sp.",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Meliscaeva.cinctella",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Orthonevra.geniculata",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Other.bee",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- NA

#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################

# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

insect_sampling <- tibble(
  study_id = "Malus_domestia_Norway_2013",
  site_id = data_obs_guild$Field,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "sweep net",
  abundance = data_obs_guild$abundance,
  total_sampled_area = 50*6*4,
  total_sampled_time = 5*6*4,
  total_sampled_flowers = 250*6*4,
  Description = "In each field abundance of floral visitors was analysed by sweep-netting all visitors along six 25 m long and 2 m wide transects for five minutes each, for a total of 30 minutes of sampling. Sampling was further repeated on at least four dates during the main flowering period")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Malus_domestia_Norway_2013.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>% rename(Site_name=Field) %>%
  filter(!is.na(abundance)) %>%
  group_by(Site_name,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  filter(!is.na(Guild)) %>%
  group_by(Site_name,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "bumblebees"  "other_flies" "syrphids"   

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(honeybees=0,other_wild_bees=0,humbleflies=0,
                                          beetles=0, non_bee_hymenoptera=0,
                                          lepidoptera=0, other =0,
                                          humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data_raw <- data_raw %>% left_join(abundance_aux,by="Site_name")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>% dplyr::rename(Site_name=Field) %>% 
  filter(!is.na(Guild)) %>%
  select(Site_name,Organism_ID,abundance)%>%
  group_by(Site_name,Organism_ID) %>% count(wt=abundance)

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

richness_aux <- abundace_field %>% select(Site_name,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data_raw <- data_raw %>% left_join(richness_aux,by="Site_name")
###############################
# FIELD LEVEL DATA
###############################


field_level_data <- tibble(
  study_id = "Malus_domestia_Norway_2013",
  site_id = data_raw$Site_name,
  crop = data_raw$Crop_species,
  variety = data_raw$Crop_variety,
  management = NA,
  country = data_raw$Country,
  latitude = data_raw$Latitude_decimal.degrees,
  longitude = data_raw$Longitude_decimal.degrees,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month = NA,
  sampling_end_month = NA,
  sampling_year = data_raw$Year,
  field_size = data_raw$Field_size_ha,
  yield=data_raw$SiteMean_Yield,
  yield_units=NA,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight= NA,
  plant_density=10000*data_raw$Number_plants_ha,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=data_raw$observed_pollinator_richness,
  other_pollinator_richness=data_raw$other_pollinator_richness,
  other_richness_estimator_method=data_raw$other_pollinator_richness,
  richness_restriction = data_raw$richness_restriction,
  abundance = data_raw$total,
  ab_honeybee = data_raw$honeybees,
  ab_bombus = data_raw$bumblebees,
  ab_wildbees = data_raw$other_wild_bees,
  ab_syrphids = data_raw$syrphids,
  ab_humbleflies= data_raw$humbleflies,
  ab_other_flies= data_raw$other_flies,
  ab_beetles=data_raw$beetles,
  ab_lepidoptera=data_raw$lepidoptera,
  ab_nonbee_hymenoptera=data_raw$non_bee_hymenoptera,
  ab_others = data_raw$other,
  total_sampled_area = 50*6*4,
  total_sampled_time = 5*6*4,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data_raw$total*100*60/(6*4*250)/(6*4*5),
  visit_honeybee = data_raw$honeybees*100*60/(6*4*250)/(6*4*5),
  visit_bombus = data_raw$bumblebees*100*60/(6*4*250)/(6*4*5),
  visit_wildbees = data_raw$other_wild_bees*100*60/(6*4*250)/(6*4*5),
  visit_syrphids = data_raw$syrphids*100*60/(6*4*250)/(6*4*5),
  visit_humbleflies = data_raw$humbleflies*100*60/(6*4*250)/(6*4*5),
  visit_other_flies = data_raw$other_flies*100*60/(6*4*250)/(6*4*5),
  visit_beetles = data_raw$beetles*100*60/(6*4*250)/(6*4*5),
  visit_lepidoptera = data_raw$lepidoptera*100*60/(6*4*250)/(6*4*5),
  visit_nonbee_hymenoptera = data_raw$non_bee_hymenoptera*100*60/(6*4*250)/(6*4*5),
  visit_others = data_raw$other*100*60/(6*4*250)/(6*4*5),
  Publication = "10.1126/science.aac7287",
  Credit = "Jens Åström",
  Email_contact = "jens.astrom@nina.no"
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Malus_domestia_Norway_2013.csv")
setwd(dir_ini)

# NOTES
# We assumed that visit to 100 flower were measured for 5 minites in each field
# J.A. richness estimations for each field refers to the average richness per round. Here we
# use total richness for four rounds

# Unidentified bees were remove from abundance and richness calculations
# (10 over 202 observations)


####################3
# JENSENS RICHNESS
#####################

div.raw<-read.csv("Norway/apple_raw_div_2013.csv")
library(reshape)
#names(div.raw)
poll.div.melt<-melt(div.raw,measure.vars=4:length(div.raw))
poll.div.sample<-cast(poll.div.melt,formula=Field+Recording~variable,fun.aggregate=sum)
poll.div.field.mean<-tapply(rowSums(poll.div.sample[3:length(poll.div.sample)]>0),
                            poll.div.sample$Field,mean)
out<-data.frame("Field"=names(poll.div.field.mean),
                "SiteMean_Richness_NetSamp"=poll.div.field.mean,
                "Minutes_NetSampling"=30)


######################################################
# REPLICATING RICHNESS
######################################################

r_test <- data_raw_obs_ini %>% select(-Plot.nr) %>% group_by(Field,Recording) %>%
  summarise_all(sum)

r_test[,3:37][r_test[,3:37]>0] <- 1

# abundace_field <- data_raw[,c(3,42:64)] %>% 
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

r_test[is.na(r_test)] <- 0
r_test$r_obser <-  0
r_test$r_chao <-  0

for (i in 1:nrow(r_test)) {
  x <- as.numeric(r_test[i,3:(ncol(r_test)-2)])
  
  r_test$r_obser[i] <-  sum(x>0)
  r_test$r_chao[i] <-  sum(x>0)
}

r_test %>% select(Field,r_obser) %>% group_by(Field) %>% summarise_all(mean)
