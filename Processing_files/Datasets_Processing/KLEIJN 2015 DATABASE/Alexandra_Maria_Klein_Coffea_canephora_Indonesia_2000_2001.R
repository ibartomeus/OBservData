
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
#library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 75_AlexKleinCoffee_2Varieties_2001
##########################

data_raw <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/74_75_AlexKleinCoffee_2Varieties_2001/Klein_Coffea canephora_flower visitation_raw.xls",
                       sheet = "SuEinzeltieren")

data_raw <- as_tibble(data_raw)

# Remove extra rows

data_raw <- data_raw[1:45,1:36]


# There should be 24 sites
data_raw %>% group_by(Site) %>% count()


##############
# Data site
##############

data.site <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/74_75_AlexKleinCoffee_2Varieties_2001/KoordinatenFl.xls",
                        sheet = "Sheet1")

data.site$latitude <- -parse_lat(data.site$S, format = NULL)
data.site$longitude <- parse_lon(data.site$E, format = NULL)

data.site <- data.site %>% select(Flaeche,latitude,longitude) %>%
  rename(site_id=Flaeche)

# Select the sites (for this study) and their corresponding coordinates

data.site <- data_raw %>% group_by(Site) %>% count() %>% select(-n) %>%
  rename(site_id=Site) %>%
  left_join(data.site,by="site_id")


# Add information

data.site$study_id <- "Alexandra_Maria_Klein_Coffea_canephora_Indonesia_2000_2001"
data.site$crop <- "Coffea canephora"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Indonesia"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_start_month <- 10
data.site$sampling_end_month <- 7
data.site$sampling_year <- "2000/2001"
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
data.site$Publication <- "10.1046/j.1365-2664.2003.00847.x"
data.site$Credit <- "Alexandra-Maria Klein, Ingolf Steffan-Dewenter and Teja Tscharntke"
data.site$Email_contact <- "alexandra.klein@nature.uni-freiburg.de"



###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>% select(-`Observation date`,-`Forest distance`) %>% rename(site_id=Site)

# Rearrange data
full_name <- colnames(data_raw_obs)
data_raw_obs <- data_raw_obs %>% gather("Organism_ID_simple",
                                        "abundance",
                                        full_name[2:length(full_name)]) %>%
  filter(abundance>0)

# Add proper taxonomic identification

tax_ID <- read_excel("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/74_75_AlexKleinCoffee_2Varieties_2001/Klein_Coffea canephora_flower visitation_raw.xls",
                       sheet = "EditsDavid")

tax_ID <- tax_ID[1:33,c(1,5)] %>% as_tibble()
tax_ID <- tax_ID %>% rename(Organism_ID_simple = `Inclusief Apis cerana`,
                            Organism_ID = `Exclusief Apis cerana`)

tax_ID$Organism_ID[tax_ID$Organism_ID_simple=="Apis cera"] <- "Apis cerana"

data_raw_obs <- data_raw_obs %>% left_join(tax_ID,by="Organism_ID_simple") %>%
  select(-Organism_ID_simple)

#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#library(taxize)

#"Apis nigrocinta binghami" <- "honeybees" (as in the case of Apis Cerana)

list_organisms_guild$Guild[grepl("Apis nigrocinta binghami",list_organisms_guild$Organism_ID,ignore.case = TRUE)] <- "honeybees"
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
  study_id = "Alexandra_Maria_Klein_Coffea_canephora_Indonesia_2000_2001",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = 75,
  total_sampled_flowers = NA,
  Description = "In each agroforestry system, bee flower visitation was observed for 25 minutes on a full-blooming coffee plant. This was repeated three times for a total of 75 minutes of observation time for each of the 24 agroforestry systems"
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Alexandra_Maria_Klein_Coffea_canephora_Indonesia_2000_2001.csv")
#setwd(dir_ini)

#######################################
# VISITATION RATE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


visitation_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)



names(visitation_aux)

# There are "honeybees" "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visitation_aux <- visitation_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,bumblebees=0,
                                          total=0)
visitation_aux[is.na(visitation_aux)] <- 0
visitation_aux$total <- rowSums(visitation_aux[,c(2:ncol(visitation_aux))])

visitation_aux[,c(2:ncol(visitation_aux))] <- 60*visitation_aux[,c(2:ncol(visitation_aux))]/75/3

data.site <- data.site %>% left_join(visitation_aux, by = "site_id")

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
  mutate(other_richness_estimator_method=NA,richness_restriction="only bees")

# Since we are estimating richness from visits, we use only the observed values.

richness_aux$other_pollinator_richness <- NA

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux, by = "site_id")


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
  total_sampled_time = 75,
  visitation_rate_units = "flower visits per plant and hour",
  visitation_rate = data.site$total,
  visit_honeybee = data.site$honeybees,
  visit_bombus = data.site$bumblebees,
  visit_wildbees = data.site$other_wild_bees,
  visit_syrphids = data.site$syrphids,
  visit_humbleflies = data.site$humbleflies,
  visit_other_flies = data.site$other_flies,
  visit_beetles = data.site$beetles,
  visit_lepidoptera = data.site$lepidoptera,
  visit_nonbee_hymenoptera = data.site$non_bee_hymenoptera,
  visit_others = data.site$other,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Alexandra_Maria_Klein_Coffea_canephora_Indonesia_2000_2001.csv")
#setwd(dir_ini)

# As in the case of Apis cerana, we assigned "Apis nigrocinta binghami" to "honeybees" guild.
