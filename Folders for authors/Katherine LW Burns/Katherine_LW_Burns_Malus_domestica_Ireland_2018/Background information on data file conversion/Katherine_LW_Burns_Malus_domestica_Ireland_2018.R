
library(tidyverse)
library(openxlsx)
library("iNEXT")

dir_ini <- getwd()

data.site <- read.xlsx("globalcrop.APPLE.BurnsStanley.xlsx",
                          sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)

data.site$study_id <- "Katherine_LW_Burns_Malus_domestica_Ireland_2018"

data.site$Publication <- NA

data.site$Credit <- data.site$Credit[1]

data.site$email <- data.site$email[1]

# Sanity check names
data.site %>% group_by(site_id) %>% count() #All site_id are unique

 


data.insect <- read.xlsx("globalcrop.APPLE.BurnsStanley.xlsx",
                       sheet = "insect_sampling", startRow = 1)
data.insect <- as_tibble(data.insect)

data.insect$study.id <- "Katherine_LW_Burns_Malus_domestica_Ireland_2018"

data.insect <- data.insect %>% rename(
                                      study_id = study.id,
                                      site_id = site.id,
                                      sampling_method = sampling.method,
                                      total_sampled_area = total.sampled.area,
                                      total_sampled_time = total.sampled.time,
                                      total_sampled_flowers = total.sampled.flowers,
                                      Description = `Description.(Fee.text)`
                                    )

data.insect$guild[data.insect$guild == "non-bee hymenoptera"]  <- "non_bee_hymenoptera" 
data.insect$guild[data.insect$guild == "other flies"]  <- "other_flies"
data.insect$guild[data.insect$guild == "other wild bees"]  <- "wild_bees"
##############################################################
# ABUNDANDE
##############################################################

abundance_aux <- data.insect %>% filter(sampling_method =="transects") %>% 
  group_by(site_id,guild) %>% count(wt=abundance) %>% 
  spread(key=guild, value=n) %>% rename()

names(abundance_aux)


# There are "beetles"             "bumblebees"          "honeybees"          
# "lepidoptera"         "non_bee_hymenoptera" "other_flies"         "syrphids"           
# "wild_bees"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(other=0, humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.insect %>% filter(sampling_method=="transects") %>%
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


# Load our estimation for taxonomic resolution

tax_res <- read_csv("taxon_table_burn01.csv")
#Mutate pollinator labels to match those of taxon table
tax_estimation <- data.insect %>%
  left_join(tax_res, by="pollinator")
tax_estimation %>% group_by(rank) %>% count()

percentage_species_morphos <- 
  sum(tax_estimation$rank %in% c("morphospecies","species"))/nrow(tax_estimation)

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos < 0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}


data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################################################
# AREA + TIME
###############################################################

sampling_aux <- data.insect %>% group_by(site_id,sampling_method) %>% 
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
  study_id = data.insect$study_id,
  site_id = data.insect$site_id,
  pollinator = data.insect$pollinator,
  guild = data.insect$guild,
  sampling_method = data.insect$sampling_method,
  abundance = data.insect$abundance,
  total_sampled_area = data.insect$total_sampled_area,
  total_sampled_time = data.insect$total_sampled_time,
  total_sampled_flowers = data.insect$total_sampled_flowers,
  Description = data.insect$Description
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Katherine_LW_Burns_Malus_domestica_Ireland_2018.csv")
setwd(dir_ini)

###############################################################
###############################################################
# FIELD LEVEL
###############################################################
###############################################################


field_level_data <- tibble(
  study_id=data.site$study_id,
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=data.site$variety,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=data.site$sampling_start_month,
  sampling_end_month=data.site$sampling_end_month,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=data.site$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
  fruits_per_plant=data.site$mean_fruits_per_plant,
  fruit_weight=data.site$fruit_weight,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  abundance=data.site$total,
  ab_honeybee=data.site$honeybees,
  ab_bombus=data.site$bumblebees,
  ab_wildbees=data.site$wild_bees,
  ab_syrphids=data.site$syrphids,
  ab_humbleflies=data.site$humbleflies,
  ab_other_flies=data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others=data.site$other,
  total_sampled_area=data.site$total_sampled_area.y,
  total_sampled_time=data.site$total_sampled_time.y,
  visitation_rate_units="visits per 100 flowers and hour",
  visitation_rate=data.site$visitation_rate,
  visit_honeybee=data.site$visit_honeybee,
  visit_bombus=data.site$visit_bombus,
  visit_wildbees=data.site$visit_wildbees,
  visit_syrphids=data.site$visit_syrphids,
  visit_humbleflies=data.site$visit_humbleflies,
  visit_other_flies=data.site$visit_other_flies,
  visit_beetles=data.site$visit_beetles,
  visit_lepidoptera=data.site$visit_lepidotera,
  visit_nonbee_hymenoptera=data.site$visit_nonbee_hymenoptera,
  visit_others=data.site$visit_others,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Katherine_LW_Burns_Malus_domestica_Ireland_2018.csv")
setwd(dir_ini)
