
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 66_AlexKleinAlmond_2009_2011
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/66_AlexKleinAlmond_2009/Final_almond_flower visits_all bee database_observ_2009_species ID.xls",
                       sheet = "data")
data_raw <- as_tibble(data_raw)

# Extract sampling month

data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$Date, format="%Y/%m/%d"),"%m"))


# There should be 15 sites
data_raw %>% group_by(Site) %>% count() # OK!

# Adapt variety information

data_raw$`Bloom Variety`[data_raw$Site=="Bramlett"] <- "Neplus + Nonpareil"
data_raw$`Bloom Variety`[data_raw$Site=="Full Belly"] <- "Neplus + Nonpareil"
data_raw$`Bloom Variety`[data_raw$Site=="Taber"] <- "Monterey + Nonpareil + Peerless"

##############
# Data site
##############

data.site <- data_raw  %>%
  select(Site,Lat,Long,`Bloom Variety`) %>%
  group_by(Site,Lat,Long,`Bloom Variety`) %>% count() %>% select(-n) %>%
  rename(site_id=Site,latitude=Lat,longitude=Long,variety=`Bloom Variety`)


# We add data site ID

data.site$study_id <- "Alexandra_Maria_Klein_Prunus_dulcis_USA_2009"
data.site$crop <- "Prunus dulcis"
data.site$management <- NA
data.site$country <- "USA"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- 10
data.site$sampling_year <- 2009
data.site$field_size <- NA
data.site$yield <- NA
data.site$yield_units <- NA
data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA
data.site$Publication <- "10.1038/ncomms8414"
data.site$Credit <- "Alexandra-Maria Klein"
data.site$Email_contact <- "alexandra.klein@nature.uni-freiburg.de"

# Add sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)

for (i in sites){

  data.site$sampling_start_month[data.site$site_id==i] <-
    data_raw %>% filter(Site==i) %>%
    select(month_of_study) %>% min()

  data.site$sampling_end_month[data.site$site_id==i] <-
    data_raw %>% filter(Site==i) %>%
    select(month_of_study) %>% max()
}


###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>%
  select(Site,Visitor,Abundance,`Frequency (total # flowers visited)`) %>% rename(site_id=Site,Organism_ID=Visitor,
                                                                                  abundance=Abundance,
                                                                                  flowers_visited=`Frequency (total # flowers visited)`)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Fruit fly"] <- "other_flies"
list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"

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
  study_id = "Alexandra_Maria_Klein_Prunus_dulcis_USA_2009",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = 5*8*20/60, #netting+observations
  total_sampled_flowers = NA,
  Description = "On each orchard, 5 trees were observed. At each tree, eight groups of flowers were observed for 20 seconds each (total of around 13 min per orchard)."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Alexandra_Maria_Klein_Prunus_dulcis_USA_2009.csv")
#setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "bumblebees"      "honeybees"       "other_flies"     "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>%
  select(site_id,Organism_ID,abundance)%>%
  group_by(site_id,Organism_ID) %>% count(wt=abundance)

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
percentage_species_morphos <- 0.9

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="mostly bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


###############################
# VISITATION RATE
###############################

# Add flowers

flowers_visited <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=flowers_visited) %>%
  spread(key=Guild, value=n)

names(flowers_visited)

flowers_visited <- flowers_visited %>% mutate(lepidoptera=0,beetles=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,
                                          total=0)
flowers_visited[is.na(flowers_visited)] <- 0
flowers_visited$total <- rowSums(flowers_visited[,c(2:ncol(flowers_visited))])

flowers_visited <- flowers_visited %>%
  mutate(
    visit_bumblebees=60*bumblebees/(40/3),
    visit_honeybees=60*honeybees/(40/3),
    visit_other_wild_bees=60*other_wild_bees/(40/3),
    visit_lepidoptera=60*lepidoptera/(40/3),
    visit_beetles=60*beetles/(40/3),
    visit_other_flies=60*other_flies/(40/3),
    visit_syrphids=60*syrphids/(40/3),
    visit_other=60*other/(40/3),
    visit_humbleflies=60*humbleflies/(40/3),
    visit_non_bee_hymenoptera=60*non_bee_hymenoptera/(40/3),
    visit_total=60*total/(40/3)
  ) %>%
  select(site_id, visit_bumblebees,visit_honeybees,visit_other_wild_bees,visit_lepidoptera,
         visit_beetles,visit_other_flies,visit_syrphids,visit_other,visit_humbleflies,
         visit_non_bee_hymenoptera,visit_total)

data.site <- data.site %>% left_join(flowers_visited, by = "site_id")
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
  total_sampled_area = NA,
  total_sampled_time = 40/3,
  visitation_rate_units = "visited flowers per hour",
  visitation_rate = data.site$visit_total,
  visit_honeybee = data.site$visit_honeybees,
  visit_bombus = data.site$visit_bumblebees,
  visit_wildbees = data.site$visit_other_wild_bees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_non_bee_hymenoptera,
  visit_others = data.site$visit_other,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Alexandra_Maria_Klein_Prunus_dulcis_USA_2009.csv")
#setwd(dir_ini)

# NOTE: what is the meaning of column "# Flower Obs."?
# How should I aggregate visitors and frequency to obtain visitation rates
# [in counts/(100 flowers hour)] per guild and site?
