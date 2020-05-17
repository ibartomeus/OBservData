
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 90_ShaleneJhaCoffee_2006
##########################

data_raw <- read_excel("90_ShaleneJhaCoffee_2006/Coffee-visit-2006-kleijn2_coordinates.xls",
                       sheet = "Visitation")
data_raw <- as_tibble(data_raw)

data_raw$zone_UTM <- 15

# Transforma UTM coodinates to lat/long in degrees

sputm <- SpatialPoints(data_raw[,c(1,2)], proj4string=CRS("+proj=utm +zone=15 +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
data_raw$longitude <- NA
data_raw$latitude <- NA
data_raw[,36:37] <- spgeo@coords

# Extract sampling months

data_raw$month_of_study <- as.numeric(format(as.Date(data_raw$date, format="%Y/%m/%d"),"%m"))

# There should be 13 sites
data_raw %>% group_by(`study site`,`UTMs-East`,`UTMs- North`) %>% count() #Only 9 sites
data_raw %>% group_by(`study site`,`Obs period`) %>% count() 

##############
# Data site
##############


data.site <- data_raw %>% select(`study site`,latitude,longitude,`UTMs-East`,`UTMs- North`) %>% 
  group_by(`study site`,latitude,longitude,`UTMs-East`,`UTMs- North`) %>% count() %>% select(-n) %>%
  rename(site_id=`study site`,X_UTM = `UTMs-East`,Y_UTM = `UTMs- North`)

data.site$study_id <- "90_ShaleneJhaCoffee_2006"
data.site$crop <- "Coffea arabica/robusta"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "Mexico"
data.site$zone_UTM <- 15
data.site$sampling_year <- 2006
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
data.site$Publication <- "10.1111/j.1600-0706.2009.17523.x"
data.site$Credit <- "Shalene Jha and John H. Vandermeer"
data.site$Email_contact <- "sjha@austin.utexas.edu"
  
data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)

for (i in sites){
  
  data.site$sampling_start_month[data.site$site_id==i] <- 
    data_raw %>% filter(`study site`==i) %>% 
    select(month_of_study) %>% min()
  
  data.site$sampling_end_month[data.site$site_id==i] <- 
    data_raw %>% filter(`study site`==i) %>% 
    select(month_of_study) %>% max()
}

###########################
# SAMPLING DATA
###########################


data_raw_obs <- data_raw %>%
  select(`study site`,`Obs period`,date,`duration in seconds`,`Species code`,
         `total flowers visited`,`Time start`) %>% 
  rename(site_id=`study site`, Organism_ID=`Species code`,
         flowers_visited=`total flowers visited`, visit_duration = `duration in seconds`)

#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

list_organisms_guild$Guild[list_organisms_guild$Organism_ID=="Apis"] <- "honeybees"
list_organisms_guild$Organism_ID[list_organisms_guild$Organism_ID %in% 
                                   c("Xylocopa tabaniformis, tabaniformis",
                                     "Xylocopa tabaniformis, tabaniformis  tabaniformis, tabaniformis")] <- 
  "Xylocopa tabaniformis"

data_raw_obs$Organism_ID[data_raw_obs$Organism_ID %in% 
                                   c("Xylocopa tabaniformis, tabaniformis",
                                     "Xylocopa tabaniformis, tabaniformis  tabaniformis, tabaniformis")] <- 
  "Xylocopa tabaniformis"

list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"

# Sanity Checks
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#Add guild to observations
data_obs_guild <- data_raw_obs %>% left_join(list_organisms_guild, by = "Organism_ID")


#######################
# INSECT SAMPLING
#######################


# Remove entries with zero abundance
data_obs_guild  <- data_obs_guild  %>% filter(flowers_visited>0)


# Check the duration of observation periods. They should be around 15 minutes
sampling_data <- data_obs_guild %>% select(-Organism_ID,-Guild) %>%
  separate(`Time start`,c("date_start","time")," ")%>%
  separate(time,c("hour","min","sec"),":") %>%
  mutate(new_time_start=3600*as.numeric(hour)+60*as.numeric(min)+as.numeric(sec),
         new_time_end = new_time_start+visit_duration)

sites <- sampling_data %>% select(site_id,`Obs period`) %>% unique()

sites$minutes_sampled <- NA


for (i in 1:nrow(sites)){
  
  duration_i <- c()
  
  sampling_i <- sampling_data %>% filter(site_id==sites$site_id[i],
                                         `Obs period`==sites$`Obs period`[i]) 
  
  for (j in 1:nrow(sampling_i)){
    
    sampling_ij <- sampling_i$new_time_start[j]:sampling_i$new_time_end[j]
    duration_i <- c(duration_i,sampling_ij)
  }

  sites$minutes_sampled[i] <- length(unique(duration_i))/60
  
}

# Check Sanity
sites$minutes_sampled #Ir1	70	15.6833333 seems to meet the 15 min condition.
# Differences could be due to time slots without observations

insect_sampling <- tibble(
  study_id = "90_ShaleneJhaCoffee_2006",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = "observation",
  abundance = data_obs_guild$flowers_visited,
  total_sampled_area = NA,
  total_sampled_time = 15,
  total_sampled_flowers = NA, #We dont know the number of observed periods per site
  Description = paste0("Abundance here refers to total amount of flowers visited by an organism. Surveys were conducted in 15 minute periods between 8:00 and 14:00. At each site, four fully flowering branches were randomly selected from a randomly chosen cofffe bush.",
                       "Observation period: ",data_obs_guild$`Obs period`)
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_90_ShaleneJhaCoffee_2006.csv")
setwd(dir_ini)

#######################################
# Visitation rate
#######################################

# According to the paper, Within the two agroforestry habitat types, 124 observation
# sessions were conducted

sessions_per_site <- sites %>% group_by(site_id) %>% count()

sum(sessions_per_site$n) # We observe 114 out of 124 (reported) sessions

# Total observation time
sessions_per_site <- sessions_per_site %>% mutate(total_time=15*n)


# Add flower visits per site

data_obs_guild_2 <-  data_obs_guild %>% select(site_id,Organism_ID,Guild,flowers_visited) %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)

# Remove entries with unknow guilds
data_obs_guild_2  <- data_obs_guild_2  %>% filter(!is.na(Guild),Guild!="NA")

visits_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=flowers_visited) %>% 
  spread(key=Guild, value=n)

names(visits_aux)

# There are "bumblebees"          "honeybees"           "non_bee_hymenoptera"
# "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visits_aux <- visits_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,bumblebees=0,
                                    syrphids=0,other=0,humbleflies=0,
                                    non_bee_hymenoptera=0,total=0)
visits_aux[is.na(visits_aux)] <- 0
visits_aux$total <- rowSums(visits_aux[,c(2:ncol(visits_aux))])

# Visitation rate: flowers visited per hour of sampling time

for (i in 1:nrow(visits_aux)){
  visits_aux[i,2:12] <- 60*visits_aux[i,2:12]/sessions_per_site$total_time[i]
}

data.site <- data.site %>% left_join(visits_aux, by = "site_id")

data.site <- data.site %>% left_join(sessions_per_site[,c(1,3)], by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Estimating species richness from flowers visited

abundace_field <- data_obs_guild %>%
  select(site_id,Organism_ID,flowers_visited)%>%
  group_by(site_id,Organism_ID) %>% count(wt=flowers_visited)

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
  mutate(other_pollinator_richness=NA,
         other_richness_estimator_method=NA,richness_restriction="only bees")

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
  ab_beetles= NA,
  ab_lepidoptera=NA,
  ab_nonbee_hymenoptera=NA,
  ab_others = NA,
  total_sampled_area = NA,
  total_sampled_time = data.site$total_time,
  visitation_rate_units = "flowers visited per hour",
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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_90_ShaleneJhaCoffee_2006.csv")
setwd(dir_ini)

# According to the paper, Within the two agroforestry habitat types, 124 observation
# sessions were conducted, 15 min each. However we only found 114 observations.

# On the other hand, our estimations point out that there are sessions longer than 15 min.
# Nonetheless, we used 15-min per session to perform our calculations.