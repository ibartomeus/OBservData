
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Ramo01: ramo01
##########################

# Load new field_level_data provided by Davi and Felipe

field_level_aux <- read.csv2("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/field_level_data_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv",dec = ".",
                             stringsAsFactors = F)

field_level_aux <- as_tibble(field_level_aux)


# Adapt DOI

field_level_aux$Publication <- "10.1371/journal.pone.0204460"

# Fix latitude and longitude

# field_level_aux$latitude <- field_level_aux$latitude/1000000
# field_level_aux$latitude[c(8,31,32)] <- field_level_aux$latitude[c(8,31,32)]*10
#
# field_level_aux$longitude <- field_level_aux$longitude/1000000
# field_level_aux$longitude[c(5,6,17,27,34)] <- field_level_aux$longitude[c(5,6,17,27,34)]*10
###################
# INSECT SAMPLING
###################

# Load insect_sampling provided by Davi and Felipe

insect_sampling_aux <- read_csv2("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/insect_sampling_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv")

# Fix description

insect_sampling_aux$Description <- paste(insect_sampling_aux$Description,". ","Temporal replicates: 2Lumped sampling rounds per plot in one wet season.")
insect_sampling <- insect_sampling_aux %>% select(-X11,-X12)

# Give proper format to columns to operate with them
insect_sampling <- insect_sampling %>% mutate(abundance=as.numeric(abundance))


# Sanity check sites
field_level_aux$site_id[!field_level_aux$site_id %in% insect_sampling$site_id]
unique(insect_sampling$site_id[!insect_sampling$site_id %in% field_level_aux$site_id])
# There are 3 fields without visitation records:"W2P1N" "W2P0N" "W2P0S"


# Sanity check guilds
insect_sampling %>% group_by(guild) %>% count()

# Fix duplicated guild names

insect_sampling$guild[insect_sampling$guild=="Bumblebees"] <- "bumblebees"

insect_sampling$Description[insect_sampling$pollinator %in%
                            c("Polybia ignobilis",
                              "Brachygastra lecheguana","Polybia dimidiata",
                              "Polybia ignobilis",
                              "Pepsis sp.","Allograpta exotica cf.")] <-
  paste0("These organisms were classified as biocontrol agents. However, since they were also registered as floral visitors, they may be potential pollinators as well. ",insect_sampling$Description[insect_sampling$pollinator %in%
                                                                                                                                                                                                         c("Polybia ignobilis",
                                                                                                                                                                                                           "Brachygastra lecheguana","Polybia dimidiata",                                                                                                                                                                                                         "Polybia ignobilis",
                                                                                                                                                                                                           "Pepsis sp.","Allograpta exotica cf.")])
insect_sampling$study_id <- "Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016"

# Save insect_sampling file

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv")
# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################
abundance_aux <- insect_sampling %>%
  group_by(site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

names(abundance_aux)

# There are bumblebees","honeybees",non_bee_hymenoptera"
#other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,other=0,
                                          other_flies=0,humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])


######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- insect_sampling %>%
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

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

###############################################################
###############################################################
###############################################################
###############################################################

# Add abundance and richness information

field_level_data_aux <- field_level_aux %>%
  select(-observed_pollinator_richness,
         -other_pollinator_richness,
         -other_richness_estimator_method,
         -richness_restriction,
         -abundance,
         -ab_honeybee,
         -ab_bombus,
         -ab_wildbees,
         -ab_syrphids,
         -ab_humbleflies,
         -ab_other_flies,
         -ab_beetles,
         -ab_lepidoptera,
         -ab_nonbee_hymenoptera,
         -ab_others) %>%
  left_join(abundance_aux,by="site_id") %>%
  left_join(richness_aux,by="site_id") %>%
  mutate(richness_restriction=NA)


# Reorder columns

field_level_data <-   tibble(
  study_id="Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016",
  site_id=field_level_data_aux$site_id,
  crop=field_level_data_aux$crop,
  variety=field_level_data_aux$variety,
  management=field_level_data_aux$management,
  country=field_level_data_aux$country,
  latitude=field_level_data_aux$latitude,
  longitude=field_level_data_aux$longitude,
  X_UTM=field_level_data_aux$X_UTM,
  Y_UTM=field_level_data_aux$Y_UTM,
  zone_UTM=field_level_data_aux$zone_UTM,
  sampling_start_month=field_level_data_aux$sampling_start_month,
  sampling_end_month=field_level_data_aux$sampling_end_month,
  sampling_year=field_level_data_aux$sampling_year,
  field_size=field_level_data_aux$field_size,
  yield=field_level_data_aux$yield,
  yield_units=field_level_data_aux$yield_units,
  yield2=field_level_data_aux$yield2,
  yield2_units=field_level_data_aux$yield2_units,
  yield_treatments_no_pollinators=field_level_data_aux$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=field_level_data_aux$yield_treatments_pollen_supplement,
  yield_treatments_no_pollinators2=field_level_data_aux$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=field_level_data_aux$yield_treatments_pollen_supplement2,
  fruits_per_plant=field_level_data_aux$fruits_per_plant,
  fruit_weight=field_level_data_aux$fruit_weight,
  plant_density=field_level_data_aux$plant_density,
  seeds_per_fruit=field_level_data_aux$seeds_per_fruit,
  seeds_per_plant=field_level_data_aux$seeds_per_plant,
  seed_weight=field_level_data_aux$seed_weight,
  observed_pollinator_richness=field_level_data_aux$observed_pollinator_richness,
  other_pollinator_richness=field_level_data_aux$other_pollinator_richness,
  other_richness_estimator_method=field_level_data_aux$other_richness_estimator_method,
  richness_restriction=NA,
  abundance=field_level_data_aux$total,
  ab_honeybee=field_level_data_aux$honeybees,
  ab_bombus=field_level_data_aux$bumblebees,
  ab_wildbees=field_level_data_aux$other_wild_bees,
  ab_syrphids=field_level_data_aux$syrphids,
  ab_humbleflies=field_level_data_aux$humbleflies,
  ab_other_flies=field_level_data_aux$other_flies,
  ab_beetles=field_level_data_aux$beetles,
  ab_lepidoptera=field_level_data_aux$lepidoptera,
  ab_nonbee_hymenoptera=field_level_data_aux$non_bee_hymenoptera,
  ab_others=field_level_data_aux$other,
  total_sampled_area=field_level_data_aux$total_sampled_area,
  total_sampled_time=NA,
  visitation_rate_units = NA,
  visitation_rate=NA,
  visit_honeybee=NA,
  visit_bombus=NA,
  visit_wildbees=NA,
  visit_syrphids=NA,
  visit_humbleflies=NA,
  visit_other_flies=NA,
  visit_beetles=NA,
  visit_lepidoptera=NA,
  visit_nonbee_hymenoptera=NA,
  visit_others=NA,
  Publication=field_level_data_aux$Publication,
  Credit=field_level_data_aux$Credit,
  Email_contact=field_level_data_aux$Email_contact
)


# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Davi_L_Ramos_Phaseolus_vulgaris L_Brazil_2015_2016.csv")
# setwd(dir_ini)

