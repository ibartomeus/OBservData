# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read.xlsx("Processing_files/Datasets_Processing/GARIBALDI 2015 DATABASE/Nacho-Lucas/datos/Jessica_Pumpkin_fruit.xlsx")
data.site <- as_tibble(data.site)

###########################
# ADD INFORMATION TO SITES
###########################

data.site <- data.site %>% rename(site_id=site,yield=fruit.set)

data.site$yield <- data.site$yield
data.site$yield_units <- "Fruit weight per plant"


data.site$study_id <- "Jessica_D_Petersen_Cucurbita_pepo_USA_2011"

data.site$crop <- "Cucurbita pepo"
data.site$variety <- "Gladiator"
data.site$management <- NA
data.site$country <- "USA"
data.site$latitude <- NA
data.site$longitude <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <- 7
data.site$sampling_end_month <- 8
data.site$sampling_year <- 2011
data.site$field_size <- NA

data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_pollen_supplement <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <- data.site$yield
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA
data.site$Publication <- "10.1111/1365-2664.12287"
data.site$Credit <- "Jessica D. Petersen and Brian A. Nault"
data.site$Email_contact <- "jessica.petersen@cornell.edu"

###########################
# SAMPLING DATA
###########################

data_raw_obs <- read_excel("Processing_files/Datasets_Processing/GARIBALDI 2015 DATABASE/Nacho-Lucas/datos/Jessica_Pumpkin_abundance.xls",
                           sheet = "Hoja5") %>%
  rename(abundance=visits,site_id=site,Organism_ID=species) %>% filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()


#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################

#The number of bees visiting pumpkin flowers in each field was counted three times. Each round three 88-m-long transects. Each transect was surveyed for 10 min

# Bee visits to pumpkin flowers were assessed visually in three transects
# in each field. A transect consisted of two rows of plants that
# were each 44 m long. The number of bees visiting pumpkin flowers
# in each field was counted three times (rounds) during the
# blooming period (2011: 16 July-23 August; 2012: 19 July-30 August).
# Sampling was conducted between 0600 and 1100 h (when flowers were open) on sunny
# to partly cloudy days with minimal wind (<15 km h1).
# Each transect was surveyed for 10 min by
# walking between the rows and counting visits of each bee species
# to pumpkin flowers, and the total number of flowers. A 'visit' was
# recorded if the bee contacted the reproductive structures.
# Average bee visitation frequency per flower was calculated in
# the following manner. For each sampling round and bee species,
# numbers of visits and flowers were summed across the three transects.
# The total number of visits was divided by the total number
# of flowers to achieve a visitation frequency for each bee species
# per sampling round. Visitation frequencies were averaged across
# the three sampling rounds and log transformed to meet normality
# assumptions. The average number of bees visiting pumpkin flowers
# is referred to as 'bee visitation frequency' throughout the
# manuscript.

# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

insect_sampling <- tibble(
  study_id = "Jessica_D_Petersen_Cucurbita_pepo_USA_2011",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = 3*3*2*44,
  total_sampled_time = 3*3*10,
  total_sampled_flowers = NA,
  Description = "Abundance information refers to average flower visits (frequency). The number of bees visiting pumpkin flowers in each field was counted three times. In each round, three 88 m-long transects. Each transect was surveyed for 10 min. The total number of visits was divided by the total number of flowers to achieve a visitation frequency for each bee species per sampling round. Visitation frequencies were averaged across the three sampling rounds")

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Jessica_D_Petersen_Cucurbita_pepo_USA_2011.csv")
#setwd(dir_ini)


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
  #chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  sum(x>0)
  abundace_field$r_chao[i] <-  NA
}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method=NA,richness_restriction="Only bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}


data.site <- data.site %>% left_join(richness_aux,by="site_id")

# VISITATION RATE
###############################

# Add flowers

flowers_visited <- data_obs_guild %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(flowers_visited)

# Guilds:
# "beetles"             "bumblebees"          "honeybees"
# "humbleflies"         "lepidoptera"         "non_bee_hymenoptera" "other"
# "other_flies"         "other_wild_bees"     "syrphids"

flowers_visited <- flowers_visited %>% mutate(beetles=0,humbleflies=0,lepidoptera=0,
                                              non_bee_hymenoptera=0,other=0,
                                              other_flies=0,syrphids=0,total=0)
flowers_visited[is.na(flowers_visited)] <- 0
flowers_visited$total <- rowSums(flowers_visited[,c(2:ncol(flowers_visited))])

# Total sampling time is 30 min per round in total

flowers_visited <- flowers_visited %>%
  mutate(
    visit_bumblebees=60*100*bumblebees/30,
    visit_honeybees=60*100*honeybees/30,
    visit_other_wild_bees=60*100*other_wild_bees/30,
    visit_lepidoptera=60*100*lepidoptera/30,
    visit_beetles=60*100*beetles/30,
    visit_other_flies=60*100*other_flies/30,
    visit_syrphids=60*100*syrphids/30,
    visit_other=60*100*other/30,
    visit_humbleflies=60*100*humbleflies/30,
    visit_non_bee_hymenoptera=60*100*non_bee_hymenoptera/30,
    visit_total=60*100*total/30
  ) %>%
  select(site_id, visit_bumblebees,visit_honeybees,visit_other_wild_bees,visit_lepidoptera,
         visit_beetles,visit_other_flies,visit_syrphids,visit_other,visit_humbleflies,
         visit_non_bee_hymenoptera,visit_total)

data.site <- data.site %>% left_join(flowers_visited, by = "site_id")
##

scale(flowers_visited$visit_total) # Z-SCORE DATA IS OK


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
  other_richness_estimator_method=data.site$other_pollinator_richness,
  richness_restriction = data.site$richness_restriction,
  abundance = NA,
  ab_honeybee = NA,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies= NA,
  ab_other_flies= NA,
  ab_beetles=NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = 3*3*2*44,
  total_sampled_time = 3*3*10,
  visitation_rate_units = "visits per 100 flowers and hour",
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


# Load data revised template


field_level_data_rev <- read_csv("Processing_files/Datasets_Processing/GARIBALDI 2015 DATABASE/Nacho-Lucas/datos/field_level_data_Jessica_D_Petersen_Cucurbita_pepo_USA_2011.csv")
field_level_data_rev <- as_tibble(field_level_data_rev)

# Fix management

field_level_data_rev$management <- "conventional"

field_level_data_rev <- mutate(field_level_data_rev, abundance = ab_honeybee + ab_bombus + ab_wildbees)

# Fix visitation rates

field_level_data_rev$visitation_rate_units = field_level_data_rev$visitation_rate_units
field_level_data_rev$visitation_rate = field_level_data_rev$visitation_rate
field_level_data_rev$visit_honeybee = field_level_data_rev$visit_honeybee
field_level_data_rev$visit_bombus = field_level_data_rev$visit_bombus
field_level_data_rev$visit_wildbees = field_level_data_rev$visit_wildbees
field_level_data_rev$visit_syrphids = field_level_data_rev$visit_syrphids
field_level_data_rev$visit_humbleflies = field_level_data_rev$visit_humbleflies
field_level_data_rev$visit_other_flies = field_level_data_rev$visit_other_flies
field_level_data_rev$visit_beetles = field_level_data_rev$visit_beetles
field_level_data_rev$visit_lepidoptera = field_level_data_rev$visit_lepidoptera
field_level_data_rev$visit_nonbee_hymenoptera = field_level_data_rev$visit_nonbee_hymenoptera
field_level_data_rev$visit_others = field_level_data_rev$visit_others


#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_rev, "Processing_files/Datasets_storage/field_level_data_Jessica_D_Petersen_Cucurbita_pepo_USA_2011.csv")
#setwd(dir_ini)

#
