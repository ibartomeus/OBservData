
library(tidyverse)
library(openxlsx)
library("iNEXT")

dir_ini <- getwd()

#####################
# SITE'S INFO
#####################

data.site <- read.xlsx("Crop_pollination_database_Bloom-Crowder.xlsx",
                          sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

# Select eveything but richness, abundance and visitation rate

data.site <- data.site[,c(1:26,54:56)]

# Update entries

data.site$crop <- "Fragaria x ananassa"
data.site$country <- "USA"


data.site$management[data.site$management=="Cerified Organic"] <- "organic"
data.site$management[data.site$management=="Organic Practices"] <- "organic"

data.site$Credit <- "Elias H. Bloom (bloomel1@msu.edu); Elisabeth C. Oeller (elisabeth.oeller@wsu.edu)"
data.site$email <- "dcrowder@wsu.edu"

# Sanity check names
data.site %>% group_by(site_id) %>% count() %>% filter(n>1) #All site_id (per year) are unique

##############################
# INSECT SAMPLING
##############################


data.insect <- read.xlsx("Crop_pollination_database_Bloom-Crowder.xlsx",
                       sheet = "insect_sampling", startRow = 1)
data.insect <- as_tibble(data.insect)

data.insect %>% group_by(guild) %>% count()

#Fix header

names(data.insect)[names(data.insect)=="Description_(fee_text)"] <- "Description"

# Fix guilds names
data.insect$guild[data.insect$guild=="bumblebee"] <- "bumblebees"
data.insect$guild[data.insect$guild=="honeybee"] <- "honeybees"    
data.insect$guild[data.insect$guild=="other_wild_bee"] <- "other_wild_bees"

# Fix time units (from hours to minutes)??

data.insect$total_sampled_time <- 60*data.insect$total_sampled_time 

##############################################################
# ABUNDANDE
##############################################################

data.insect %>% group_by(sampling_method) %>% count()

abundance_aux <- data.insect %>% filter(sampling_method =="sweep_net") %>% 
  group_by(site_id,guild) %>% count(wt=abundance) %>% 
  spread(key=guild, value=n) %>% rename()

names(abundance_aux)


# There are "bumblebees"          "honeybees"          "other_wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,non_bee_hymenoptera=0,other_flies=0,
                                          syrphids=0,other=0, humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# To estimate richness (CHAO) and abundance we only use transects/sweep_net counts

abundace_field <- data.insect %>% filter(sampling_method=="sweep_net") %>%
  select(site_id,pollinator,abundance)%>%
  group_by(site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,2:(ncol(abundace_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  chao$Observed
  abundace_field$r_chao[i] <-  chao$Estimator 
}

# Percentage of species+morphos:
percentage_species_morphos <- 1

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao 1",richness_restriction="Only bees")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}



data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################################################
# AREA + TIME
###############################################################

sampling_aux <- data.insect %>% filter(sampling_method=="sweep_net") %>%
  group_by(site_id,sampling_method) %>% 
  summarize(total_sampled_area=mean(total_sampled_area),
            total_sampled_time=mean(total_sampled_time)
            ) %>%
  group_by(site_id) %>% 
  summarize(total_sampled_area=sum(total_sampled_area),
            total_sampled_time=sum(total_sampled_time)
  )

data.site <- data.site %>% left_join(sampling_aux, by = "site_id")

###############################################################
###############################################################
# INSECT SAMPLING
###############################################################
###############################################################



insect_sampling <- tibble(
  study_id = "David_Crowder_Fragaria_ananassa_USA_2016",
  site_id = data.insect$site_id,
  pollinator = data.insect$pollinator,
  guild = data.insect$guild,
  sampling_method = data.insect$sampling_method,
  abundance = data.insect$abundance,
  total_sampled_area = data.insect$total_sampled_area,
  total_sampled_time = data.insect$total_sampled_time,
  total_sampled_flowers = data.insect$total_sampled_flowers,
  Description = paste0(data.insect$Description,". Both traps and sweep nets were conducted over a 50m transect. Traps were left for 9 hours, sweeping was done for 1 hour.")
)

insect_sampling$Description[insect_sampling$Description=="NA. Both traps and sweep nets were conducted over a 50m transect. Traps were left for 9 hours, sweeping was done for 1 hour."] <- 
  "Both traps and sweep nets were conducted over a 50m transect. Traps were left for 9 hours, sweeping was done for 1 hour."


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_David_Crowder_Fragaria_ananassa_USA_2016.csv")
setwd(dir_ini)

###############################################################
###############################################################
# FIELD LEVEL
###############################################################
###############################################################


field_level_data <- tibble(
  study_id = "David_Crowder_Fragaria_ananassa_USA_2016",
  site_id = data.site$site_id,
  crop = data.site$crop,
  variety = data.site$variety,
  management = data.site$management,
  country = data.site$country,
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
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
  fruits_per_plant=NA,
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
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
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
  Email_contact = data.site$email
)



setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_David_Crowder_Fragaria_ananassa_USA_2016.csv")
setwd(dir_ini)
