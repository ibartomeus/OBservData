# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read.xlsx("Nacho-Lucas/datos/Mark_pigeonpea_fruit.xlsx")
data.site <- as_tibble(data.site)

###########################
# ADD INFORMATION TO SITES
###########################

data.site <- data.site %>% rename(site_id=site,yield=fruit.set)

data.site$yield <- data.site$yield
data.site$yield_units <- "Insect Pollination = Open pollination [control] - Self-pollination [Tulle bags]"

data.site$study_id <- "Mark_Otieno_Cajanus_cajan_Kenya_2009"

data.site$crop <- "Cajanus cajan"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Kenya"
data.site$latitude <- NA
data.site$longitude <- NA
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <- 4
data.site$sampling_end_month <- 6
data.site$sampling_year <- 2009
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
data.site$Publication <- "10.1007/s10841-015-9788-z"
data.site$Credit <- "Mark Otieno, C. Sheena Sidhu, Ben A. Woodcock, Andrew Wilby, Ioannis N. Vogiatzakis, Alice L. Mauchline, Mary W. Gikungu, and Simon G. Potts"
data.site$Email_contact <- "mmarkotieno@gmail.com"

###########################
# SAMPLING DATA
###########################

data_raw_obs <- read_excel("Nacho-Lucas/datos/Mark_pigeonpea_abundance.xls",
                           sheet = "Hoja5") %>%
  rename(abundance=visits,site_id=site,Organism_ID=species) %>% filter(abundance>0)


#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[grepl("Dactylurina",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Euaspis abdominalis",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("Pachymelus",list_organisms_guild$Organism_ID,ignore.case = FALSE)] <- "other_wild_bees"
#Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################

# Bee abundance was measured by observing bee visitation
# to flowers. Bees were observed along five 100 m transects
# at each pigeon pea crop field; transects were placed north to
# south, each separated by a minimum of 10 m at each site.
# Bee visitations within 2 m of the transect were recorded as
# we walked each transect for 10 min, twice a day. 
# A total of 49 days were spent to sample
# all the 12 sites between 20th April and 20th June 2009. Bee
# species richness was quantified by collecting bees and
# identifying them to species or to morphospecies.

# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)

insect_sampling <- tibble(
  study_id = "Mark_Otieno_Cajanus_cajan_Kenya_2009",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = "Abundance information refers to the number of flower visits (frequency). Bees were observed along five 100 m-long 2 m-wide transects at field. Each transect was walked twice a day. Observation time was 10 min per transect."
  )


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Mark_Otieno_Cajanus_cajan_Kenya_2009.csv")
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
  #chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  sum(x>0)
  abundace_field$r_chao[i] <-  NA 
}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- (222-76)/222

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method=NA,richness_restriction=NA)

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

flowers_visited <- flowers_visited %>% mutate(beetles=0,bumblebees=0,
                                              humbleflies=0,lepidoptera=0,non_bee_hymenoptera=0,other=0,
                                              other_flies=0,syrphids=0,total=0)
flowers_visited[is.na(flowers_visited)] <- 0
flowers_visited$total <- rowSums(flowers_visited[,c(2:ncol(flowers_visited))])

# We do not know the total sampling time per field

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
write_csv(field_level_data, "field_level_data_Mark_Otieno_Cajanus_cajan_Kenya_2009.csv")
setwd(dir_ini)


# Abundance data suggest that average values were obtained by dividing over 11.
# According to the paper's methods, the number of transects per field should be a multiple of 10.

