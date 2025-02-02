
library(tidyverse)
library("iNEXT")
library(openxlsx)
library(rgdal)

dir_ini <- getwd()
options(digits=14)
##########################
#Data: DAINESE, Fije01: fije02
##########################


data.site <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Fije01_Datacollection_pollination.xlsx",
                          sheet = "SiteData", startRow = 2)
data.site <- as_tibble(data.site)

data.site <- data.site %>%
  separate(col = SiteID, into = c("study_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])",remove = F)

data.site <- data.site %>% select(-dis)
data.site$study_id[data.site$study_id == "IT"] <- "fije02"
data.site$study_id[data.site$study_id == "FR"] <- "fije01"

data.site <- data.site %>% filter(study_id=="fije02")

#management_types <- c("Conventional"="conventional","Organic"="organic")

data.site <- data.site %>% select(-'Annual/perennial')%>%
  rename(site_id=SiteID,X_UTM=X,Y_UTM=Y,sampling_year=Year,crop=Crop.species,
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
# Note that there are 5 treatments!!!

data.Functioning <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Fije01_Datacollection_pollination.xlsx",
                                 sheet = "Functioning", startRow = 2)

data.Functioning <- as_tibble(data.Functioning)

data.Functioning <- data.Functioning %>%
  separate(col = SiteID, into = c("study_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])",remove = F)

data.Functioning <- data.Functioning %>% select(-dis)
data.Functioning$study_id[data.Functioning$study_id == "IT"] <- "fije02"
data.Functioning$study_id[data.Functioning$study_id == "FR"] <- "fije01"


data.Functioning <- data.Functioning %>% filter(study_id=="fije02") %>% select(-study_id)

data.Functioning <- data.Functioning %>% group_by(SiteID,Year.of.sampling,Type.of.function,Exclosure.treatment) %>%
  summarise(yield=mean(Function)) %>%
  rename(site_id=SiteID,sampling_year=Year.of.sampling,yield_units=Type.of.function,variety=Exclosure.treatment)

data.Functioning %>% group_by(yield_units) %>% count()

yield_aux1 <- data.Functioning %>% filter(yield_units=="Seeds produced (yield)")
yield_aux1$yield_units <- "z-score Seeds produced"
#yield_aux1$yield <- scale(yield_aux1$yield, center = TRUE, scale = TRUE)


yield_aux2 <- data.Functioning %>% filter(yield_units=="Seed set (proportion)") %>%
  rename(yield2_units=yield_units,yield2=yield)
#yield_aux2$yield2 <- 100*yield_aux2$yield2
yield_aux2$yield2_units <- "z-score Seed set (%)"
#yield_aux2$yield2 <- scale(yield_aux2$yield2, center = TRUE, scale = TRUE)

data.site <- data.site %>% left_join(yield_aux1, by = c("site_id","sampling_year"))
data.site <- data.site %>% left_join(yield_aux2, by = c("site_id","sampling_year","variety"))

###########################
# Adding  Field_size

data.site$field_size <- NA

data.site <- data.site %>% mutate(country="Italy",Publication="10.1111/ele.13150",
                                  Credit="Thijs Fijen, David Kleijn",
                                  email="thijs.fijen@wur.nl")

################################
#COLLECTING INSECT SAMPLING DATA
################################

data.species <- read.xlsx("Processing_files/Datasets_processing/DAINESE 2019 DATABASE/DATASETS/Fije01_Datacollection_pollination.xlsx",
                          sheet = "SpeciesData", startRow = 2)

data.species <- as_tibble(data.species)

data.species <- data.species %>%
  separate(col = SiteID, into = c("study_id", "dis"),"(?<=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]) ?(?=[0-9])",remove = F)

data.species <- data.species %>% select(-dis)
data.species$study_id[data.species$study_id == "IT"] <- "fije02"
data.species$study_id[data.species$study_id == "FR"] <- "fije01"

data.species <- data.species %>% rename(site_id=SiteID,sampling_year=Year.of.sampling,
                                        sampling_method=Sampling.method,abundance=Abundance,
                                        Organism_ID=OrganismID)

data.species_01 <- data.species %>% filter(study_id=="fije02")

# Evaluate the percentage of species + morphospecies
data.species_01 %>% group_by(Identified.to) %>% count()
percentage_species_morphos <-
  sum(data.species_01$Identified.to %in% c("species","morphospecies"))/nrow(data.species_01)


data.species_01 %>% group_by(sampling_method) %>% count()

gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")

data.species_01 <- data.species_01 %>% select(-Identified.to,-X6)


data.species_01 <- data.species_01 %>% left_join(gild_list,by=c("Organism_ID","Family"))
#Check NA's in guild
data.species_01 %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count()

data.species_01 <- data.species_01 %>% mutate(total_sampled_area=NA,
                                              total_sampled_time=NA,
                                              total_sampled_flowers=NA,
                                              Description=paste("Line ",Line,". 3-5 transects (total abundance + species richness, 150m2 in three subtransects, each 5 minutes pure observation time) and plant observations (visitation rate; 40 minutes per female line per field) during flowering period of primary flower heads (approximately 3 weeks). Minimum of 4 days between observations (mean 5 days). Average number of flowers for an Allium Porrum plant: 2000",sep=""))


data.species_01$total_sampled_time[data.species_01$sampling_method=="Plant observations"] <- 0.5*40*data.species_01$Number.of.censuses[data.species_01$sampling_method=="Plant observations"]
data.species_01$total_sampled_time[data.species_01$sampling_method=="Transect"] <- 15*data.species_01$Number.of.censuses[data.species_01$sampling_method=="Transect"]
data.species_01$total_sampled_area[data.species_01$sampling_method=="Transect"] <- 150*data.species_01$Number.of.censuses[data.species_01$sampling_method=="Transect"]
data.species_01$total_sampled_flowers[data.species_01$sampling_method=="Plant observations"] <- 2000*data.species_01$Number.of.censuses[data.species_01$sampling_method=="Plant observations"]

data.species_01$Description[data.species_01$sampling_method=="Transect"] <- "All female lines observed simultaneously. 3-5 transects (total abundance + species richness, 150m2 in three subtransects, each 5 minutes pure observation time) and plant observations (visitation rate; 40 minutes per female line per field) during flowering period of primary flower heads (approximately 3 weeks). Minimum of 4 days between observations (mean 5 days). Average number of flowers for an Allium Porrum plant: 2000"

data.species_01_obs <- data.species_01 %>% filter(sampling_method=="Plant observations")
data.species_01_trans <- data.species_01 %>% filter(sampling_method=="Transect")

insect_sampling_obs <- tibble(
  study_id = "Thijs_Fijen_Allium_porrum_Italy_2016",
  site_id = paste(data.species_01_obs$site_id,data.species_01_obs$Line,sep = "_"),
  pollinator = data.species_01_obs$Organism_ID,
  guild = data.species_01_obs$Guild,
  sampling_method = data.species_01_obs$sampling_method,
  abundance = data.species_01_obs$abundance,
  total_sampled_area = data.species_01_obs$total_sampled_area,
  total_sampled_time = data.species_01_obs$total_sampled_time,
  total_sampled_flowers = data.species_01_obs$total_sampled_flowers,
  Description = data.species_01_obs$Description
)

insect_sampling_trans <- NULL
sites_lines <- c("B","C","D","E","F")

for (i in 1:nrow(data.species_01_trans)){

  for (j in 1:length(sites_lines)){

    insect_sampling_trans_i <- tibble(
      study_id = "Thijs_Fijen_Allium_porrum_Italy_2016",
      site_id = paste(data.species_01_trans$site_id[i],sites_lines[j],sep = "_"),
      pollinator = data.species_01_trans$Organism_ID[i],
      guild = data.species_01_trans$Guild[i],
      sampling_method = data.species_01_trans$sampling_method[i],
      abundance = data.species_01_trans$abundance[i],
      total_sampled_area = data.species_01_trans$total_sampled_area[i],
      total_sampled_time = data.species_01_trans$total_sampled_time[i],
      total_sampled_flowers = data.species_01_trans$total_sampled_flowers[i],
      Description = data.species_01_trans$Description[i]
    )

    insect_sampling_trans <- bind_rows(insect_sampling_trans,insect_sampling_trans_i)
  }
}

insect_sampling <- bind_rows(insect_sampling_obs,insect_sampling_trans)


# Some female lines are not present in every site. We filter insect_sampling once we got all the
# site ID from the field_level_data dataframe

# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
# write_csv(insect_sampling, "insect_sampling_Thijs_Fijen_Allium_porrum_Italy_2016.csv")
#
# setwd(dir_ini)


#########################################
#PROCESSING INSECT SAMPLING FOR FIELD DATA
#########################################

# Aggregating female lines

data.species_aux <- data.species_01 %>% group_by(site_id,Organism_ID,Guild,sampling_method) %>%
  summarise(abundance=sum(abundance),total_sampled_area=mean(total_sampled_area),
            total_sampled_time=mean(total_sampled_time))

abundance_aux <- data.species_aux %>% filter(sampling_method=="Transect") %>%
  group_by(site_id,Guild,total_sampled_time,total_sampled_area) %>% count(wt=abundance) %>%
  spread(key=Guild, value=n)

names(abundance_aux)

# There are bumblebees","honeybees","non_bee_hymenoptera",
#"other_flies", "other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

abundance_aux <- abundance_aux %>% mutate(beetles=0,lepidoptera=0,other=0,
                                          humbleflies=0,total=0)
abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(2:ncol(abundance_aux))])

data.site <- data.site %>% left_join(abundance_aux, by = "site_id")

######################################################
# ESTIMATING CHAO INDEX
######################################################

# Para estimar la riqueza (CHAO) y la abundancia solo vamos a utilizar los transectos

abundace_field <-  data.species_aux %>% filter(sampling_method=="Transect") %>%
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


#########################################
#PROCESSING INSECT SAMPLING FOR VISITATION RATE
#########################################

data.census <- data.species_01 %>% group_by(site_id,Number.of.censuses,sampling_method,Line) %>%
  count() %>% select(-n)

data.observation <- data.census %>% filter(sampling_method!="Transect") %>%
  mutate(flowers=2000*Number.of.censuses,time_obs=0.5*40*Number.of.censuses)

data.observation <- data.observation %>% group_by(site_id, Line) %>% summarise(total_time=sum(time_obs),
                                                                               total_flowers=sum(flowers))

data.species_aux2 <- data.species_01 %>% group_by(site_id,Organism_ID,Guild,sampling_method,Line) %>%
  summarise(abundance=sum(abundance),total_sampled_area=mean(total_sampled_area),
            total_sampled_time=mean(total_sampled_time))

visit_aux <- data.species_aux2 %>% filter(sampling_method!="Transect") %>%
  group_by(site_id,Guild,Line) %>% summarise(n=sum(abundance)) %>%
  spread(key=Guild, value=n)

visit_aux <- data.observation %>% left_join(visit_aux,by=c("site_id","Line"))

# Transformation to visits/100flowers/hour

for (i in 1:nrow(visit_aux)){

  visit_aux[i,5:ncol(visit_aux)] <- 60*100*visit_aux[i,5:ncol(visit_aux)]/(visit_aux$total_time[i]*visit_aux$total_flowers[i])

}


names(visit_aux)

# There are bumblebees","honeybees","non_bee_hymenoptera",
#other_wild_bees","syrphids"

# GUILDS:honeybees, bumblebees, other wild bees, syrphids, humbleflies,
# other flies, beetles, non-bee hymenoptera, lepidoptera, and other

visit_aux <- visit_aux %>% mutate(beetles=0,lepidoptera=0,other=0,other_flies=0,
                                  humbleflies=0,total=0)
visit_aux[is.na(visit_aux)] <- 0
visit_aux$total <- rowSums(visit_aux[,c(5:ncol(abundance_aux))])

names(visit_aux)

#Adapting variable names to template ones

visit_aux <- visit_aux %>% rename(visitation_rate=total,
                                  visit_honeybee=honeybees,
                                  visit_bombus=bumblebees,
                                  visit_wildbees=other_wild_bees,
                                  visit_syrphids=syrphids,
                                  visit_humbleflies=humbleflies,
                                  visit_other_flies=other_flies,
                                  visit_beetles=beetles,
                                  visit_lepidoptera=lepidoptera,
                                  visit_nonbee_hymenoptera=non_bee_hymenoptera,
                                  visit_others=other,
                                  total_time_obser=total_time,
                                  total_flowers_obser=total_flowers,
                                  variety=Line)

data.site <- data.site %>% left_join(visit_aux, by = c("site_id","variety"))
###############################################################
###############################################################
###############################################################
###############################################################


field_level_data <- tibble(
  study_id="Thijs_Fijen_Allium_porrum_Italy_2016",
  site_id=data.site$site_id,
  crop=data.site$crop,
  variety=data.site$variety,
  management=data.site$management,
  country=data.site$country,
  latitude=NA,
  longitude=NA,
  X_UTM=NA,
  Y_UTM=NA,
  zone_UTM=NA,
  sampling_start_month=5,
  sampling_end_month=6,
  sampling_year=data.site$sampling_year,
  field_size=data.site$field_size,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=data.site$yield2,
  yield2_units=data.site$yield2_units,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
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
  richness_restriction = "Bees, syrphids and wasps",
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
  total_sampled_area=data.site$total_sampled_area,
  total_sampled_time=data.site$total_sampled_time,
  visitation_rate_units = "visits per 100 flowers and hour",
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

field_level_data$site_id <- paste(field_level_data$site_id,field_level_data$variety,sep = "_")


# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "Processing_files/Datasets_storage/field_level_data_Thijs_Fijen_Allium_porrum_Italy_2016.csv")
# setwd(dir_ini)

# It seems that some female lines are not present in some fields (87 site_id < 5 fem lines*18 sites)
# Consequently, we filter insect_sampling (because transect data were calculated assuming
# that every line was included in each field)

insect_sampling_real <- insect_sampling %>% filter(site_id %in% field_level_data$site_id)
# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_real, "Processing_files/Datasets_storage/insect_sampling_Thijs_Fijen_Allium_porrum_Italy_2016.csv")
# setwd(dir_ini)

