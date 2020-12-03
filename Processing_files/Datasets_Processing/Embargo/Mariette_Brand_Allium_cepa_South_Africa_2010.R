
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
#library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 86_MarietteBrandOnion_2010
##########################

data.site <- read.xlsx("85_86_MarietteBrandOnion_2009_2010/Distances SA onion.xlsx",
                       sheet = "Distances",startRow = 11)

data.site <- data.site[1:12,1:11]

data.site <- as_tibble(data.site)

# Tranforms date to proper units
data.site$Sampling.Day <- openxlsx::convertToDate(data.site$Sampling.Day)
data.site$month_of_study <- as.numeric(format(as.Date(data.site$Sampling.Day, format="%Y/%m/%d"),"%m"))

# Adapt latitude and longitude information
data.site$`Latitude.(S)` <- -1*parse_lat(data.site$`Latitude.(S)`)
data.site$`Longitude.(E)` <- parse_lon(data.site$`Longitude.(E)`)

# There should be 16 sites
data.site %>%filter(Sampling.Year==2010) %>% group_by(Farm.Name) %>% count() 


##############
# Data site
##############

data.site <- data.site %>% filter(Sampling.Year==2010) %>% 
  select(Farm.Name,Variety,Sampling.Year,`Field.Size.(ha)`,
         `Latitude.(S)`,`Longitude.(E)`,month_of_study) %>%
  rename(site_id=Farm.Name,variety=Variety,
         sampling_year=Sampling.Year,field_size=`Field.Size.(ha)`,
         latitude=`Latitude.(S)`,longitude=`Longitude.(E)`,
         sampling_start_month=month_of_study
    ) %>% mutate(sampling_end_month=sampling_start_month)


# We add data site ID

data.site$study_id <- "Mariette_Brand_Allium_cepa_South_Africa_2010"
data.site$crop <- "Allium cepa"
data.site$management <- NA
data.site$country <- "South Africa"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
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
data.site$Credit <- "Mariëtte R. Brand; Ruan Veldtman"
data.site$Email_contact <- "mariettebrand@gmail.com/veldtman@sun.ac.za"

###########################
# SAMPLING DATA
###########################

# Observation list
data_raw_obs <- read.xlsx("85_86_MarietteBrandOnion_2009_2010/SA_Onion data_MarietteBrand_editted.xlsx",
                              sheet = "Observation data",startRow = 1)

data_raw_obs <- data_raw_obs[1:960,1:5]
data_raw_obs <- as_tibble(data_raw_obs)


# Select observations for the study year

data_raw_obs <- data_raw_obs %>% filter(!Site %in% c("09OvdW","09JD","09JO","09FvdM")) 

data_raw_obs %>% group_by(Site,Session,Parent) %>% count()

# Identification of "bees"
identification <- read.xlsx("85_86_MarietteBrandOnion_2009_2010/Bee Identifications_editted.xlsx",
                                       sheet = "Sheet1",startRow = 3)

identification <- identification[1:14,c(2,4,5,9)]
identification <- as_tibble(identification)
identification$Parent.line[identification$Parent.line=="Male-sterile"] <- "MS"
identification$Parent.line[identification$Parent.line=="Male-fertile"] <- "MF"
identification$Session <- as.numeric(identification$Session)
identification <- rename(identification,Parent=Parent.line)
identification$Visitor <- "Bee"

data_raw_obs <- data_raw_obs %>% left_join(identification, by=c("Site","Session","Parent","Visitor"))

data_raw_obs <- data_raw_obs %>% rename(site_id=Site,Organism_ID=Visitor,abundance=Abundance)

#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Honeybee"] <- "honeybees"
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
  study_id = "Mariette_Brand_Allium_cepa_South_Africa_2010",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = 4*10*4, 
  total_sampled_flowers = NA,
  Description = "On each observation day, observations were made during four data collection periods. Observations were replicated five times on both male-fertile and male-sterile rows during each collection period. Four neighbouring umbels in at least 50% bloom were selected and observed for 4 minutes."
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Mariette_Brand_Allium_cepa_South_Africa_2010.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>% select(site_id,Organism_ID,Guild,abundance) %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "honeybees" "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,bumblebees=0,
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
percentage_species_morphos <- 0.5

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="only bees")

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
  total_sampled_time = 4*10*4,
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
write_csv(field_level_data, "field_level_data_Mariette_Brand_Allium_cepa_South_Africa_2010.csv")
setwd(dir_ini)

# NOTE: Records in "Bee Identifications_editted" (excel file) do not match bee
# observations in "SA_Onion data_MarietteBrand_editted" (excel file) with
# abundance greater that zero. For that reason, bee identification only relies on
# "bee" and "honeybee" categories.

# We assumed that Bee entries belong to "other_wild_bees" guild
