# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read.xlsx("Nacho-Lucas/datos/Breno_cotton_fruit.xlsx")
data.site <- as_tibble(data.site)

###########################
# ADD INFORMATION TO SITES
###########################

data.site <- data.site %>% rename(site_id=site,yield=fruit.set)

data.site$yield <- data.site$yield
data.site$yield_units <- "Fruit-set (% of flowers setting fruits at harvest.)"


data.site$study_id <- "Breno_cotton"

data.site$crop <- "Gossypium hirsutum"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Brazil"
data.site$latitude <- NA
data.site$longitude <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA
data.site$sampling_year <- NA
data.site$field_size <- NA

data.site$yield2 <- NA
data.site$yield2_units <- NA
data.site$yield_treatments_no_pollinators <- NA
data.site$yield_treatments_pollen_supplement <- NA
data.site$yield_treatments_no_pollinators2 <- NA
data.site$yield_treatments_pollen_supplement2 <- NA
data.site$fruits_per_plant <- NA
data.site$fruit_weight <- NA
data.site$plant_density <- NA
data.site$seeds_per_fruit <- NA
data.site$seeds_per_plant <- NA
data.site$seed_weight <- NA
data.site$Publication <- "10.1126/science.1230200"
data.site$Credit <- "Breno M. Freitas"
data.site$Email_contact <- "freitas@ufc.br"

###########################
# SAMPLING DATA
###########################

data_raw_obs <- read.xlsx("Nacho-Lucas/datos/Breno_cotton_abundance.xlsx") %>%
  rename(abundance=visits,site_id=site,Organism_ID=species) %>% filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Psaenythia sp."] <- "other_wild_bees"

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################

# Insect visitation to flowers of acerola, annatto and cotton (tables S1 and S2) was
# assessed following the same general methodology by choosing four (annatto) to five
# (acerola and cotton) plants at random, monitoring floral visitors during a fixed period,
# and counting the flowers they visited. Plants were monitored at different times of the day
# according to anthesis and floral longevity for each plant species. Acerola flowers were
# monitored five times per day (6, 9, 12, 15, 18 h), cotton flowers six times (7, 9, 11, 13,
#                                                                              15, 17 h) 
# and annatto seven times (5, 6, 7, 8, 9, 10, 11 h) during at least six days per
# month during the blooming season. Each observation lasted 15 min and floral visitors
# were recorded, captured using sweep nets, and fixed in ethyl acetate. Later, they were
# pinned and identified. Pollination success was assessed as the percentage of flowers
# setting fruits at harvest.


insect_sampling <- tibble(
  study_id = "Breno_cotton",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "Abundance information refers to the number of flower visits (frequency). Insect visitation to flowers of cotton was assessed  by choosing five plants at random, monitoring floral visitors during a fixed period, and counting the flowers they visited. Cotton plants were monitored six times (7, 9, 11, 13, 15, 17 h) during at least six days per month during the blooming season. Each observation lasted 15 min.")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Breno_cotton.csv")
setwd(dir_ini)



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

insect_sampling$pollinator[grepl(" sp.",insect_sampling$pollinator,ignore.case = F)]

percentage_species_morphos <- (274-(12+9))/274

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_pollinator_richness=NA,other_richness_estimator_method=NA,richness_restriction=NA)

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

#  There are "honeybees"      "non_bee_hymenoptera" "other_wild_bees"

# Guilds:
# "beetles"             "bumblebees"          "honeybees"          
# "humbleflies"         "lepidoptera"         "non_bee_hymenoptera" "other"              
# "other_flies"         "other_wild_bees"     "syrphids"

flowers_visited <- flowers_visited %>% mutate(total=0,beetles=0,bumblebees=0,
                                              humbleflies=0,lepidoptera=0,
                                              other=0,
                                              other_flies=0,syrphids=0)

flowers_visited[is.na(flowers_visited)] <- 0
flowers_visited$total <- rowSums(flowers_visited[,c(2:ncol(flowers_visited))])

# Total sampling time is 80 min per orchard in total

flowers_visited <- flowers_visited %>%
  mutate(
    visit_bumblebees=bumblebees,
    visit_honeybees=honeybees,
    visit_other_wild_bees=other_wild_bees,
    visit_lepidoptera=lepidoptera,
    visit_beetles=beetles,
    visit_other_flies=other_flies,
    visit_syrphids=syrphids,
    visit_other=other,
    visit_humbleflies=humbleflies,
    visit_non_bee_hymenoptera=non_bee_hymenoptera,
    visit_total=total
  ) %>%
  select(site_id, visit_bumblebees,visit_honeybees,visit_other_wild_bees,visit_lepidoptera,
         visit_beetles,visit_other_flies,visit_syrphids,visit_other,visit_humbleflies,
         visit_non_bee_hymenoptera,visit_total)

data.site <- data.site %>% left_join(flowers_visited, by = "site_id")
##

scale(flowers_visited$visit_total) # Z-SCORE DATA IS OK

data2015.zscore <- read.delim("downloadSupplement.txt", sep=" ")
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
  total_sampled_area = NA,
  total_sampled_time = NA,
  visitation_rate_units = "visits per unit of time",
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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Breno_cotton.csv")
setwd(dir_ini)
