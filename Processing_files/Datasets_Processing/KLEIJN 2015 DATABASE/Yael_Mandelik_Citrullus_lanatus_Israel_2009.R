
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
#library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 39_PisantyMandelikWatermelon_2009
##########################

data_raw <- read.xlsx("Processing_files/Datasets_processing/KLEIJN 2015 DATABASE/39_40_PisantyMandelikWatermelon_2009_2010/crop visitors Pisanty and Mandelik_editted.xlsx",
                       sheet = "watermelon")
data_raw <- as_tibble(data_raw)

data_raw$zone_UTM <- 36

# Transforma UTM coodinates to lat/long in degrees

sputm <- SpatialPoints(data_raw[,c(4,5)], proj4string=CRS("+proj=utm +zone=36 +datum=WGS84"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
data_raw$longitude <- NA
data_raw$latitude <- NA
data_raw[,8:9] <- spgeo@coords

# Extract sampling months
data_raw$Date <- openxlsx::convertToDate(data_raw$Date)
data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$Date, format="%Y/%m/%d"),"%m"))
data_raw$sampling_year <- as.numeric(format(as.Date(data_raw$Date, format="%Y/%m/%d"),"%Y"))


# Data for our study year
data_raw <- data_raw %>% filter(sampling_year==2009)

# There should be 17 sites
fields <- data_raw %>% group_by(latitude,longitude,UTM.36R.E,UTM.36R.N) %>% count() %>%
  select(-n)

# Create an ID for each field
fields$site_id <- seq.int(nrow(fields))

# Add ID to data_raw
data_raw <- data_raw %>% left_join(fields, by=c("latitude","longitude","UTM.36R.E","UTM.36R.N"))

##############
# Data site
##############


data.site <- data_raw %>% select(site_id,latitude,longitude,UTM.36R.E,UTM.36R.N) %>%
  group_by(site_id,latitude,longitude,UTM.36R.E,UTM.36R.N) %>% count() %>% select(-n) %>%
  rename(X_UTM = UTM.36R.E,Y_UTM = UTM.36R.N)

data.site$study_id <- "Yael_Mandelik_Citrullus_lanatus_Israel_2009"
data.site$crop <- "Citrullus lanatus"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Israel"
data.site$zone_UTM <- 36
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
data.site$Publication <- "10.1890/14-0910.1"
data.site$Credit <- "Gideon Pisanty, Yael Mandelik"
data.site$Email_contact <- "gidpisa79@yahoo.com / Yael.mandelik@mail.huji.ac.il"

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)

for (i in sites){

  data.site$sampling_start_month[data.site$site_id==i] <-
    data_raw %>% filter(site_id==i) %>%
    select(month_of_study) %>% min()

  data.site$sampling_end_month[data.site$site_id==i] <-
    data_raw %>% filter(site_id==i) %>%
    select(month_of_study) %>% max()
}

###########################
# SAMPLING DATA
###########################

# There are fields with extra 25x25 m2 plots. Since we don't know their corresponding IDs,
# we set to NA the sampling parameters.

sampling <- data_raw %>%
  select(site_id,Date) %>% unique() %>% group_by(site_id) %>% count() %>%
  mutate(total_area=NA*n*2,total_time=NA*n*2)


data_raw_obs <- data_raw %>%
  select(site_id,species) %>% mutate(abundance=1) %>% rename(Organism_ID=species)



#Add guild via guild list

gild_list_raw <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

# Those species belong to ""other_wild_bees"" guild
list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"

# Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(abundance>0)


insect_sampling <- tibble(
  study_id = "Yael_Mandelik_Citrullus_lanatus_Israel_2009",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "netting",
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA, #We dont know the number and IDs of fieds with two plotsper site
  Description = "Fields have one or two study plots (25 x 25 m). Each plot was sampled between one and three times (mostly twice), each time on a separate day. In each sampling day, two sampling sessions were carried out. Each session included 15 min of bee netting."
)

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Yael_Mandelik_Citrullus_lanatus_Israel_2009.csv")
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

# There are ""other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(honeybees=0,lepidoptera=0,beetles=0,other_flies=0,
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
percentage_species_morphos <- 0.9

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1",richness_restriction="only non-managed bees")

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
  total_sampled_time = NA,
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


##############
# UPDATE
field_level_data$variety <- "Malali"
field_level_data$management <- "conventional"

#setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Yael_Mandelik_Citrullus_lanatus_Israel_2009.csv")
#setwd(dir_ini)

# There are fields with two (25x25 m2) plots. Since we don't know their corresponding IDs,
# we set to NA the sampling parameters, namely: total sampling area and total sampling time
