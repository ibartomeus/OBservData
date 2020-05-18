
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
#library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 14_MikeGarratt_FieldBean2005
##########################

data_raw_transect <- read.xlsx("14_MikeGarratt_FieldBean2005/Field bean 2005 data for David.xlsx",
                       sheet = "1.Transect data")

data_raw_transect$method <- "transect"
data_raw_transect$Date <- openxlsx::convertToDate(data_raw_transect$Date,origin = "1904-01-01")
data_raw_transect$month_of_study <- as.numeric(format(as.Date(data_raw_transect$Date, format="%Y/%m/%d"),"%m"))
data_raw_transect$abundance <- 1

data_raw_transect <- data_raw_transect %>% 
  rename(site_id=Name.of.the.site,Organism_ID=Name.of.the.species) %>%
  select(site_id,Organism_ID,abundance,method,month_of_study)


data_raw_pantrap <- read.xlsx("14_MikeGarratt_FieldBean2005/Field bean 2005 data for David.xlsx",
                               sheet = "2. Pan_trap")
data_raw_pantrap$method <- "pantrap"
data_raw_pantrap$Date <- parse_date(data_raw_pantrap$Date,"%d/%m/%Y")
data_raw_pantrap$month_of_study <- as.numeric(format(as.Date(data_raw_pantrap$Date, format="%d/%m/%Y"),"%m"))

data_raw_pantrap <- data_raw_pantrap %>% 
  rename(site_id=Site.name_short,abundance=No..Of.individuals,Organism_ID=Name.of.the.species) %>%
  select(site_id,Organism_ID,abundance,method,month_of_study)

data_raw <- bind_rows(data_raw_transect,data_raw_pantrap)

# Fix site_id's

data_raw$site_id[data_raw$site_id %in% c("Ewelme","Ewelme Park")] <- "Ewelme Park"
data_raw$site_id[data_raw$site_id %in% c("Haseley","Little Haseley")] <- "Little Haseley"
data_raw$site_id[data_raw$site_id %in% c("Manor Road","Manor Road Farm")] <- "Manor Road Farm"
data_raw$site_id[data_raw$site_id %in% c("Mortimer","Mortimer; Gt. Park Farm")] <- "Mortimer; Gt. Park Farm"
data_raw$site_id[data_raw$site_id %in% c("Red Lion","Red Lion Farm; Britwell Salome")] <- "Red Lion Farm; Britwell Salome"
data_raw$site_id[data_raw$site_id %in% c("Whitepond","Whitepond Farm")] <- "Whitepond Farm"
data_raw$site_id[data_raw$site_id %in% c("Fawley","S. Fawley")] <- "S. Fawley"

# There should be 10 sites
data_raw %>% group_by(site_id) %>% count() 

##############
# Data site
##############


data.site <- read.xlsx("14_MikeGarratt_FieldBean2005/Field bean 2005 data for David.xlsx",
                       sheet = "Site location") %>%
  rename(site_id=Site,latitude=Latitude,longitude=Longitude) %>% 
  select("site_id","latitude","longitude") 

data.site %>% group_by(site_id) %>% count() 
# We add data site ID

data.site$study_id <- "14_MikeGarratt_FieldBean2005"
data.site$crop <- "Vicia faba"
data.site$variety <- NA
data.site$management <- NA
data.site$country <- "UK"
data.site$X_UTM <- NA
data.site$Y_UTM <- NA
data.site$zone_UTM <- NA
data.site$sampling_year <- 2005
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
data.site$Publication <- NA
data.site$Credit <- "Michael Garratt"
data.site$Email_contact <- "m.p.garratt@reading.ac.uk"
  
# Add sampling months

data.site$sampling_start_month <- NA
data.site$sampling_end_month <- NA

sites <- unique(data.site$site_id)

str(data_raw)

for (i in sites){
  
  data.site$sampling_start_month[data.site$site_id==i] <- 
    data_raw %>% filter(site_id==i) %>% 
    select(month_of_study) %>% min(na.rm = T)
  
  data.site$sampling_end_month[data.site$site_id==i] <- 
    data_raw %>% filter(site_id==i) %>% 
    select(month_of_study) %>% max(na.rm = T)
}


###########################
# SAMPLING DATA
###########################

data_raw_obs <- data_raw %>%
  filter(abundance>0) %>% select(-month_of_study)

#Add guild via guild list

gild_list_raw <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")
gild_list <- gild_list_raw %>% select(-Family) %>% unique()

list_organisms <- select(data_raw_obs,Organism_ID) %>% unique() %>% filter(!is.na(Organism_ID))
list_organisms_guild <- list_organisms %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
list_organisms_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID) %>% count()

#library(taxize)

list_organisms_guild$Guild[grepl("bombus",list_organisms_guild$Organism_ID,ignore.case = TRUE)] <- "bumblebees"
list_organisms_guild$Guild[grepl("Cheilosia",list_organisms_guild$Organism_ID,ignore.case = TRUE)] <- "syrphids"
list_organisms_guild$Guild[grepl("Metasyrphus corollae",list_organisms_guild$Organism_ID,ignore.case = TRUE)] <- "syrphids"
list_organisms_guild$Guild[is.na(list_organisms_guild$Guild)] <- "other_wild_bees"
list_organisms_guild$Guild[grepl("UID",list_organisms_guild$Organism_ID,ignore.case = TRUE)] <- NA

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
  study_id = "14_MikeGarratt_FieldBean2005",
  site_id = data_obs_guild$site_id,
  pollinator = data_obs_guild$Organism_ID,
  guild = data_obs_guild$Guild,
  sampling_method = data_obs_guild$method,
  abundance = data_obs_guild$abundance,
  total_sampled_area = NA,
  total_sampled_time = NA,
  total_sampled_flowers = NA,
  Description = NA
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_14_MikeGarratt_FieldBean2005.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

data_obs_guild_2 <-  data_obs_guild %>% filter(method=="transect",!is.na(Organism_ID)) %>%
  select(-method) %>%
  group_by(site_id,Organism_ID,Guild) %>% summarise_all(sum,na.rm=TRUE)


abundance_aux <- data_obs_guild_2 %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>% 
  spread(key=Guild, value=n)



names(abundance_aux)

# There are "bumblebees" "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(lepidoptera=0,beetles=0,other_flies=0,
                                          syrphids=0,other=0,humbleflies=0,
                                          non_bee_hymenoptera=0,honeybees=0,
                                          total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- data_obs_guild %>% filter(method=="transect",!is.na(Organism_ID)) %>%
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
x <- data_obs_guild %>% filter(method=="transect",!is.na(Organism_ID)) %>%
  group_by(Organism_ID) %>% count()


percent <- 1 - sum(x$n[x$Organism_ID=="Bombus Sp.  "])/sum(x$n)


percentage_species_morphos <- percent

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

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_14_MikeGarratt_FieldBean2005.csv")
setwd(dir_ini)

# I found differences between this dataset and the one from Potts (in Dainese 2019). Please, confirm
# that the referred datasets are different.
