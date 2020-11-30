
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Clas01: clas01
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Clas01_Datacollection_pollination_Corrected.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site$study_id <- "Alice_Classen_Coffea_arabica_Tanzania_2011_2012"
data.site$X <- as.numeric(data.site$X)
data.site$Y <- as.numeric(data.site$Y)

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,longitude=X,latitude=Y,sampling_year=Year,crop=Crop.species,
         management=Management)

#data.site$management <- unname(management_types[data.site$management])

##################################################
# TRANFORMATION FROM UTM TO DEGREES
# It is not possible to translate UTM to long lat without the corresponding UTM zone number/ID
# https://mangomap.com/robertyoung/maps/69585/what-utm-zone-am-i-in-#

#sputm <- SpatialPoints(data.site[,3:4], proj4string=CRS("+proj=utm +zone=24 +datum=WGS84"))
#spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))

#####################################
# YIELD
# Note that there are 2 treatments

data.Functioning <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Clas01_Datacollection_pollination_Corrected.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning[1:484,]

# Proper fruit set
data.Functioning$Function[data.Functioning$Type.of.function=="late fruit set"] <-
  data.Functioning$X10[data.Functioning$Type.of.function=="late fruit set"]

# Yield = late fruit set

# No exclosures: select "open" AND "vert excl"

yield <- data.Functioning %>% filter(Type.of.function=="late fruit set",
                            Exclosure.treatment %in% c("open","vert excl" )) %>%
  group_by(SiteID) %>% summarise(yield=mean(Function,na.rm = T))

# No pollinators

yield_treatments_no_pollinators <- data.Functioning %>%
  filter(Type.of.function=="late fruit set",
         Exclosure.treatment %in% c("poll excl","poll vert excl")) %>%
  group_by(SiteID) %>% summarise(yield_treatments_no_pollinators=mean(Function,na.rm = T))


# Yield 2: fruit weight

yield2 <- data.Functioning %>% filter(Type.of.function=="fruit weight",
                                     Exclosure.treatment %in% c("open","vert excl" )) %>%
  group_by(SiteID) %>% summarise(yield2=mean(Function,na.rm = T))

# No pollinators

yield_treatments_no_pollinators2 <- data.Functioning %>%
  filter(Type.of.function=="fruit weight",
         Exclosure.treatment %in% c("poll excl","poll vert excl")) %>%
  group_by(SiteID) %>% summarise(yield_treatments_no_pollinators2=mean(Function,na.rm = T))

yield_final <- yield %>%
  left_join(yield_treatments_no_pollinators,by="SiteID")%>%
  left_join(yield2,by="SiteID")%>%
  left_join(yield_treatments_no_pollinators2,by="SiteID") %>%
  mutate(yield=100*yield,
         yield_treatments_no_pollinators=100*yield_treatments_no_pollinators,
         yield_treatments_pollen_supplement=NA,
         yield_treatments_pollen_supplement2=NA,
         yield_units="late fruit set (%)",
         yield2_units="fruit weight"
  ) %>%
  rename(site_id=SiteID)



data.site <- data.site %>% left_join(yield_final, by = c("site_id"))

###########################
# Adding  Field_size

data.site$field_size <- 0.25

data.site <- data.site %>% mutate(country="Tanzania",Publication="10.1098/rspb.2013.3148, http://dx.doi.org/10.5281/zenodo.12540",
                                  Credit="Alice Classen, Ingolf Steffan-Dewenter (University of W?rzburg). Landscape Data: Thomas Nauss et al. (University of Marburg)",
                                  email="alice.classen@uni-wuerzburg.de")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Clas01_Datacollection_pollination_Corrected.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)
data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species

# Evaluate the percentage of species + morphospecies
data.species_01 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_01$Identified.to %in% c("morphospecies","species"))/nrow(data.species_01)


data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_01 <- data.species_01 %>% select(-Identified.to,-X6)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

#NA appears in Guild due to spaces in the excel entries > FIX

data.species_01 <- data.species_01 %>% filter(!is.na(Guild))
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description="8 trap clusters per site and season (cluster = white + yellow + blue trap)/10-min observations on 17 flowering coffee bushes")

data.species_01$total_sampled_time[data.species_01$sampling_method=="observation"] <- 170


insect_sampling <- tibble(
  study_id = "Alice_Classen_Coffea_arabica_Tanzania_2011_2012",
  site_id = data.species_01$site_id,
  pollinator = data.species_01$Organism_ID,
  guild = data.species_01$Guild,
  sampling_method = data.species_01$sampling_method,
  abundance = data.species_01$abundance,
  total_sampled_area = data.species_01$total_sampled_area,
  total_sampled_time = data.species_01$total_sampled_time,
  total_sampled_flowers = data.species_01$total_sampled_flowers,
  Description = data.species_01$Description
)

# Add sampled flowers

flowers <- read_csv2("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/insect_sampling_Alice_Claßen_Coffea_arabica_Tanzania_2011_2012.csv")

# Sanity check

all(flowers[,c(2,3,5)]==insect_sampling[,c(2,3,5)])

insect_sampling$total_sampled_flowers <- flowers$total_sampled_flowers


# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "Processing_files/Datasets_storage/insect_sampling_Alice_Classen_Coffea_arabica_Tanzania_2011_2012.csv")

# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################


abundance_aux <- data.species_01 %>% filter(sampling_method=="observation") %>%
  group_by(site_id,Guild) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are honeybees","lepidoptera","non_bee_hymenoptera",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,bumblebees=0,other=0,
                                          humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <- data.species_01 %>% filter(sampling_method=="observation") %>%
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

richness_aux <- abundace_field %>% select(site_id,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

if (percentage_species_morphos<0.8){
  richness_aux[,2:ncol(richness_aux)] <- NA
}
data.site <- data.site %>% left_join(richness_aux, by = "site_id")
###############################################################
###############################################################
#VISITATION RATE

visit_aux <- abundance_aux

visit_aux[,2:ncol(visit_aux)] <- 60*visit_aux[,2:ncol(visit_aux)]/(17*170)

visit_aux <- visit_aux %>% rename(

  visitation_rate= total,
  visit_honeybee= honeybees,
  visit_bombus= bumblebees,
  visit_wildbees= other_wild_bees,
  visit_syrphids= syrphids,
  visit_humbleflies= humbleflies,
  visit_other_flies= other_flies,
  visit_beetles= beetles,
  visit_lepidoptera= lepidoptera,
  visit_nonbee_hymenoptera= non_bee_hymenoptera,
  visit_others= other,
)
data.site <- data.site %>% left_join(visit_aux, by = "site_id")



###############################################################
###############################################################


field_level_data <- tibble(
  study_id=data.site$study_id,
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=NA,
  management=data.site$management,
  country=data.site$country,
  latitude=data.site$latitude,
  longitude=data.site$longitude,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=NA,
  sampling_end_month=NA,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=NA,
  fruit_weight=NA,
  plant_density=NA,
  seeds_per_fruit=NA,
  seeds_per_plant=NA,
  seed_weight=NA,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  abundance=data.site$total,
  ab_honeybee=data.site$honeybees,
  ab_bombus=data.site$bumblebees,
  ab_wildbees=data.site$other_wild_bees,
  ab_syrphids=data.site$syrphids,
  ab_humbleflies=data.site$humbleflies,
  ab_other_flies=data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others=data.site$other,
  total_sampled_area=NA,
  total_sampled_time=170,
  visitation_rate_units="visits per hour and bush",
  visitation_rate=data.site$visitation_rate,
  visit_honeybee=data.site$visit_honeybee,
  visit_bombus=data.site$visit_bombus,
  visit_wildbees=data.site$visit_wildbees,
  visit_syrphids=data.site$visit_syrphids,
  visit_humbleflies=data.site$visit_humbleflies,
  visit_other_flies=data.site$visit_other_flies,
  visit_beetles=data.site$visit_beetles,
  visit_lepidoptera=data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera=data.site$visit_nonbee_hymenoptera,
  visit_others=data.site$visit_others,
  Publication=data.site$Publication,
  Credit=data.site$Credit,
  Email_contact=data.site$email
)

# Adding Alice's additional data

add_data <- read_csv2("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/field_level_data_Alice_Claßen_Coffea_arabica_Tanzania_2011_2012.csv")

# Sanity check

field_level_data$site_id==add_data$site_id

# Add sampling months

field_level_data$sampling_start_month <- add_data$sampling_start_month
field_level_data$sampling_end_month <- add_data$sampling_end_month

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Alice_Classen_Coffea_arabica_Tanzania_2011_2012.csv")
# setwd(dir_ini)

