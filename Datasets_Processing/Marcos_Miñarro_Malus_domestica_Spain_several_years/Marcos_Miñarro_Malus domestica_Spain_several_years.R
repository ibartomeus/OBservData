# load libraries
library(tidyverse)
library("iNEXT")
library(readxl)
library(openxlsx)

dir_ini <- getwd()

# Load data


data.site <- read_excel("Crop_pollination_database_ Cider_Apple.xlsx", sheet = "field_level_data")
data.site <- as_tibble(data.site)

data.site %>% group_by(site_id,sampling_year) %>% count()

# New study ID
data.site$study_id[data.site$sampling_year==2015] <- "Marcos_Miñarro_Malus_domestica_Spain_2015"
data.site$study_id[data.site$sampling_year==2016] <- "Marcos_Miñarro_Malus_domestica_Spain_2016"
# Crop Latin name
data.site$crop <- "Malus domestica" 
# Rename management
data.site$management[data.site$management=="Organic Certified Agriculture"] <- "organic"
data.site$management[data.site$management=="Integrated pest management"] <- "IPM"

# Fix field size
data.site$field_size <- data.site %>% 
  separate(field_size,c("field_size","unit"),"ha") %>% select(field_size)



# Publication, credit, and email
data.site$Publication <- "10.1007/s13592-018-0600-4"
data.site$Credit <- "Marcos Miñarro, Daniel García"
data.site$email <- "mminarro@serida.org"


###########################
# SAMPLING DATA
###########################

insect_sampling <- read_excel("Crop_pollination_database_ Cider_Apple.xlsx", sheet = "insect_sampling")

# New study ID
insect_sampling$study_id[insect_sampling$study_id=="Mina2015"] <- "Marcos_Miñarro_Malus_domestica_Spain_2015"
insect_sampling$study_id[insect_sampling$study_id=="Mina2016"] <- "Marcos_Miñarro_Malus_domestica_Spain_2016"

# Fix area and time
insect_sampling$total_sampled_area <- 47.12
insect_sampling$total_sampled_time <- 75

# Fix guilds
insect_sampling$guild[insect_sampling$guild=="beetle"] <-"beetles"
insect_sampling$guild[insect_sampling$guild=="non-bee hymenoptera"] <-"non_bee_hymenoptera"
insect_sampling$guild[insect_sampling$guild=="other flies"] <-"other_flies"
insect_sampling$guild[insect_sampling$guild=="other wild bees"] <-"other_wild_bees"
insect_sampling$guild[insect_sampling$pollinator=="Agrypnus murinus"] <- "beetles"
insect_sampling %>% group_by(guild) %>% count()

# Add sampling info to data.site

data.site$total_sampled_area <- 47.12
data.site$total_sampled_time <- 75

# Modify Description
insect_sampling <- insect_sampling %>% rename(Description=`Description_(fee_text)`)
insect_sampling$Description <- "Each orchard was surveyed three times. Each survey was the observation of 5 trees * 5 minutes (25 min). In each tree, we observed an area of about 1m of radius in which number of flowers was counted. When possible, insects were identified to species or caught for a posterior identificaction. An additional capture (10 minutes at each survey) of pollinators visiting apple flowers was made for identification in the lab of species not identifyied in the field sampling. This info was not used for abundance (that is why we only considered plant observaction as sasmpling method) but just to assign relative abundances to specific species from general groups (e.g. wild bees). This is why in some cases total abundances have decimal values. An example: we counted 1 wild bee during surveys but we could not identify the species on the wing. Then we captured 2 wild bees that were identified as L. pauxilium and L. zonulum. Then we assigned abundance 0,5 to each species (1 wild bee/ 2 species)."

# Save new insect sampling templates

insect_sampling_2015 <- insect_sampling %>% filter(study_id=="Marcos_Miñarro_Malus_domestica_Spain_2015")
insect_sampling_2016 <- insect_sampling %>% filter(study_id!="Marcos_Miñarro_Malus_domestica_Spain_2015")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_2015, "insect_sampling_Marcos_Miñarro_Malus_domestica_Spain_2015.csv")
write_csv(insect_sampling_2016, "insect_sampling_Marcos_Miñarro_Malus_domestica_Spain_2016.csv")
setwd(dir_ini)

#######################################
# ABUNDANCE
#######################################

# Add site observations

abundance_aux <- insect_sampling %>% select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>% 
  spread(key=guild, value=n)

names(abundance_aux)

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))


######################################################
# ESTIMATING CHAO INDEX
######################################################

abundace_field <- insect_sampling %>%
  select(study_id, site_id,pollinator,abundance)%>%
  group_by(study_id,site_id,pollinator) %>% count(wt=abundance)

abundace_field <- abundace_field %>% spread(key=pollinator,value=n)

# abundace_field <- data_raw[,c(3,42:64)] %>% 
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

abundace_field[is.na(abundace_field)] <- 0
abundace_field$r_obser <-  0
abundace_field$r_chao <-  0

for (i in 1:nrow(abundace_field)) {
  x <- as.numeric(abundace_field[i,3:(ncol(abundace_field)-2)])
  #chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundace_field$r_obser[i] <-  sum(x>0)
  abundace_field$r_chao[i] <-  NA 
}


# Load our estimation for taxonomic resolution
percentage_species_morphos <- .8

richness_aux <- abundace_field %>% select(study_id,site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% dplyr::rename(observed_pollinator_richness=r_obser,
                                               other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method=NA,richness_restriction=NA)

if (percentage_species_morphos < 0.8){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id"))

##################
# VISITATION RATES
##################

flowers <- insect_sampling %>% group_by(study_id,site_id) %>% summarise(flowers=mean(total_sampled_flowers))

visit_aux <- abundance_aux %>% left_join(flowers,by=c("study_id","site_id")) %>%
  mutate(
    vist_beetles = 60*100*beetles/75/flowers,
    vist_bumblebees = 60*100*bumblebees/75/flowers,
    vist_honeybees = 60*100*honeybees/75/flowers,
    vist_humbleflies = 60*100*humbleflies/75/flowers,
    vist_lepidoptera = 60*100*lepidoptera/75/flowers,
    vist_non_bee_hymenoptera = 60*100*non_bee_hymenoptera/75/flowers,
    vist_other_flies = 60*100*other_flies/75/flowers,
    vist_other_wild_bees = 60*100*other_wild_bees/75/flowers,
    vist_others = 60*100*others/75/flowers,
    vist_syrphids =  60*100*syrphids/75/flowers,
    vist_total = vist_beetles + vist_bumblebees + 
      vist_honeybees + vist_humbleflies + 
      vist_lepidoptera + vist_non_bee_hymenoptera +
      vist_other_flies + vist_other_wild_bees + vist_others + vist_syrphids
  ) %>%
  select(study_id,site_id,vist_beetles,vist_bumblebees,vist_honeybees, vist_humbleflies, 
           vist_lepidoptera,vist_non_bee_hymenoptera,vist_other_flies,
         vist_other_wild_bees,vist_others,vist_syrphids,vist_total
         )

data.site <- data.site %>% left_join(visit_aux,by=c("study_id","site_id"))
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
  fruits_per_plant=data.site$mean_fruits_per_plant,
  fruit_weight= data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness.y,
  other_pollinator_richness=data.site$other_pollinator_richness.y,
  other_richness_estimator_method=data.site$other_richness_estimator_method.y,
  richness_restriction = data.site$richness_restriction.y,
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
  ab_others = data.site$others,
  total_sampled_area = data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$vist_total,
  visit_honeybee = data.site$vist_honeybees,
  visit_bombus = data.site$vist_bumblebees,
  visit_wildbees = data.site$vist_other_wild_bees,
  visit_syrphids = data.site$vist_syrphids,
  visit_humbleflies = data.site$vist_humbleflies,
  visit_other_flies = data.site$vist_other_flies,
  visit_beetles = data.site$vist_beetles,
  visit_lepidoptera = data.site$vist_lepidoptera,
  visit_nonbee_hymenoptera = data.site$vist_non_bee_hymenoptera,
  visit_others = data.site$vist_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$email
)

field_level_data_2015 <- field_level_data %>% filter(study_id=="Marcos_Miñarro_Malus_domestica_Spain_2015")
field_level_data_2016 <- field_level_data %>% filter(study_id!="Marcos_Miñarro_Malus_domestica_Spain_2015")

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data_2015, "field_level_data_Marcos_Miñarro_Malus_domestica_Spain_2015.csv")
write_csv(field_level_data_2016, "field_level_data_Marcos_Miñarro_Malus_domestica_Spain_2016.csv")
setwd(dir_ini)
