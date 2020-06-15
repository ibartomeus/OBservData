library(tidyverse)
library("iNEXT")

dir_ini <- getwd()
options(digits=14)
##########################
#Data: Bartomeus 2013: Bart01
##########################

###############################
#Site information
################################

# 6) Questions: You can extract variety from here. Management is conventional. 
# This is the  publication:
# https://www.sciencedirect.com/science/article/pii/S1439179115001000
# (for the DOI, credit, email el mio, etc...)


questions <- read.table("questions.txt", header = TRUE)
questions <- as_tibble(questions)

data.site <- questions %>% select(FieldID,Oilseed_Variety) %>%
  rename(site_id = FieldID, variety = Oilseed_Variety)

coordinates <- read.table("coordinates_OSR_fields.txt", header = TRUE)
coordinates <- as_tibble(coordinates) %>% rename(site_id = Field,
                                                 latitude = WGS84_N,longitude = WGS84_E)


data.site <- data.site %>% left_join(coordinates,by="site_id")

# Additional information

data.site$study_id <- "Ignasi_Bartomeus_Brassica_napus_Sweden_2013"
data.site$crop <- "Brassica napus"
data.site$management <- "conventional"
data.site$country <- "Sweden"
data.site$sampling_start_month <- 5
data.site$sampling_end_month <- 6
data.site$sampling_year <- 2013
data.site$field_size <- NA
data.site$Publication <- "10.1016/j.baae.2015.07.004"
data.site$Credit <- "Ignasi Bartomeus, Vesna Gagic, Riccardo Bommarco"
data.site$Email_contact <- "nacho.bartomeus@gmail.com"

###############################
# CROP YIELD
###############################

# Yield: Peso y numero de semilla por pod.


yield_raw <- read.table("yield.txt", header = TRUE)
yield_raw <- as_tibble(yield_raw)
head(yield_raw)

#Add av. numer of seeds and av. weight
yield_raw <- mutate(yield_raw,
                    av_seeds = rowSums(yield_raw[,4:23])/20.0,
                    av_weight = rowSums(yield_raw[,24:26])/300.0, #av_weight of 100 seeds
                    av_weight_seed_per_pod = av_seeds*av_weight) 

# Remove control data
data.yield <- yield_raw %>%
  filter(!Site %in% c("Crop_Edge","Control_Crop")) %>%
  select(Field, Plot,Site, av_seeds,av_weight,av_weight_seed_per_pod)

# Format Field_ID: from factor to number
data.yield$Field <- as.character(data.yield$Field)
data.yield$Field <- parse_number(data.yield$Field)

data.yield <- data.yield %>% arrange(Field,Site) %>%
  rename(site_id=Field,Treatment=Site)

####################################################
#PODS
#Beware of this file! Data should be fixed

pod_raw <- read.table("pods.txt", header = TRUE)
pod_raw <- as_tibble(pod_raw)
head(pod_raw)

# Remove control data
pod <- pod_raw %>%
  filter(!Treatment %in% c("Control_Edge","Control_Crop")) %>%
  select(Field, Plot,Treatment, Pod_number)

# Fix data problem

pod$Treatment[21:25] <- "Edge"

# Format Field_ID: from factor to number
pod$Field <- as.character(pod$Field)
pod$Field <- parse_number(pod$Field)

pod <- pod %>% arrange(Field,Treatment) %>%
  group_by(Field,Treatment) %>%
  count(Plot, wt = Pod_number/5.0) %>% rename(site_id = Field, av_pods_per_plant = n)

# Add av_pods_per_plant to data.yield

data.yield <- data.yield %>% left_join(pod, by=c("site_id","Plot","Treatment"))


####################################################
# Plant density

plant_density_raw <- read.table("plant_density.txt", header = TRUE)
plant_density_raw <- as_tibble(plant_density_raw)
head(plant_density_raw)

# Remove control data
plant_density <- plant_density_raw %>%
  filter(!Treatment %in% c("Control_Edge","Control_Crop")) %>%
  select(Field, Plot,Treatment, Plants_per_m2) %>% rename(site_id = Field)

# Format Field_ID: from factor to number
plant_density$site_id <- as.character(plant_density$site_id)
plant_density$site_id <- parse_number(plant_density$site_id)

# Add plants_per_m2 to data.yield

data.yield <- data.yield %>% left_join(plant_density, by=c("site_id","Plot","Treatment"))


#######################################
# Combining yield info

data.yield <- data.yield %>% rename(av_seeds_pod = av_seeds) %>%
  mutate(av_yield = av_weight_seed_per_pod * av_pods_per_plant * Plants_per_m2 * 10, #kg/hectare
         av_fruit_per_plant = av_seeds_pod * av_pods_per_plant,
         av_seeds_per_plant = av_seeds_pod * av_pods_per_plant)


# Summarize per site

data.yield.summ <- data.yield %>% group_by(site_id) %>% 
  summarise(yield_units="kg/ha",
            seeds_per_fruit = mean(av_seeds_pod),
            fruits_per_plant = mean(av_fruit_per_plant),
            plant_density = mean(Plants_per_m2),
            yield = mean(av_yield),
            fruits_per_plant = mean(av_pods_per_plant),
            seeds_per_plant  = mean(av_seeds_per_plant),
            seed_weight  = mean(100*av_weight)) # 100 seeds weight


data.site <- data.site %>% left_join(data.yield.summ,by=c("site_id"))

##########################
# VISITATION RATES
##########################

# pollinators De aqui puedes sacar visitation rates.
# Son counts de 5 minutos y hay el número de flores, por tanto tienes visitas
#(por grupo; por flor por tiempo).

# Tenemos #visitas por grupo/campo/tratamiento
#y el número de flores/campro/tratamiento/area

#visitation rate [visits in 100 flowers during one hour].

#AMOUNT OF VISITS PER GROUP

visit_rates <- read.table("pollinators.txt", header = TRUE)
visit_rates <- as_tibble(visit_rates)

str(visit_rates)
head(visit_rates)

#Gild categories: honeybees, bumblebees, other wild bees, syrphids, other

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

# gild_list_visits <- c("A_mellifera" = "honeybees", "Wild_Bees" = "wild_bees",
#                       "Symphyta" = "non_bee_hymenoptera","B_lapidarius" = "bumblebees",
#                       "B_terrestris" = "bumblebees", "B_lucorum" = "bumblebees",
#                       "Syrhipds" = "syrphids", "Other_Dipterans" = "other_flies",
#                       "Pieris_napi" = "lepidoptera","Cerambycidae" = "beetles",
#                       "Elateridae" = "beetles", "Curcumelidae" = "other",
#                       "Chrysomelidae" = "beetles","Pollen_Beetle_Present" = "beetles",
#                       "total" = "total")


#Add total (insect counts excluding pollen beetles -col 18-) and amount of flowers
visit_rates <- mutate(visit_rates,
                      total = rowSums(visit_rates[,5:17]),
                      flowers = Inflorescences_half_m2 * Flowers_per_Inflorescence) 

# Remove control data
visits <- visit_rates %>%
  filter(!visit_rates$Treatment %in% c("Control_Edge","Control_Crop")) %>%
  select("Field", "Treatment","A_mellifera", "Wild_Bees", "Symphyta",
         "B_lapidarius", "B_terrestris", "B_lucorum",
         "Syrhipds", "Other_Dipterans", "Pieris_napi",
         "Cerambycidae", "Elateridae", "Curcumelidae",
         "Chrysomelidae","Pollen_Beetle_Present","total","flowers") 


# amount of visits for species
# remove Pollen_Beetle_Present (suggested by Nacho)

m_visits <- gather(visits,"A_mellifera", "Wild_Bees", "Symphyta",
                        "B_lapidarius", "B_terrestris", "B_lucorum",
                        "Syrhipds", "Other_Dipterans", "Pieris_napi",
                        "Cerambycidae", "Elateridae", "Curcumelidae",
                        "Chrysomelidae","Pollen_Beetle_Present", "total",
                        key = "variable", value = "counts") %>%
  rename(Organism_ID=variable)

gild_list %>% filter(Organism_ID %in% m_visits$Organism_ID)

# We have no information about organisms' families. For that reason,
# filtering gild_list is needed

gild_list_filt <- gild_list %>% filter(Organism_ID %in% m_visits$Organism_ID) %>%
  group_by(Organism_ID,Guild) %>% count() %>% select(-n)

# remove Pollen_Beetle_Present

m_visits_p <- m_visits %>% left_join(gild_list_filt, by=c("Organism_ID")) %>%
  filter(Organism_ID != "Pollen_Beetle_Present")


m_visits_p %>% filter(is.na(Guild)) # "total" (=Sum of organisms) has no guild assigned

# Adding organism counts per site an treatment

m_visits2 <-m_visits_p %>%    
  group_by(Field, Guild) %>% summarise(n=sum(counts))

#Add guild categories to classify visits

#AMOUNT OF FLOWERS PER FIELD/TREAT

# Remove control data
flowers_m2 <- visit_rates %>%
  filter(!visit_rates$Treatment %in% c("Control_Edge","Control_Crop")) %>% 
  select("Field", "Treatment","flowers")

flowers_3 <- flowers_m2 %>%
  group_by(Field) %>%
  count(Field, wt = flowers)

resumen_visitas <- m_visits2 %>%
  select(Field,Guild,n) %>%
  left_join(flowers_3, by = "Field")%>%
  mutate(rate = 12*100*n.x/n.y)

visitation_rates_final <- tibble(
  Field = resumen_visitas$Field,
  Guild = resumen_visitas$Guild,
  rate = resumen_visitas$rate
)

visitation_rates_final <- spread(visitation_rates_final, key = Guild, value = rate)

# Since column "<NA>" refers to total visitation rate, we change its name. Besides, we add
# humbleflies column
visitation_rates_final <-   mutate(visitation_rates_final,humbleflies=0) %>%
  rename(visitation_rate = "<NA>",site_id=Field,
         visit_honeybee = honeybees,
         visit_bombus = bumblebees,
         visit_wildbees = other_wild_bees,
         visit_syrphids = syrphids,
         visit_humbleflies = humbleflies,
         visit_other_flies = other_flies,
         visit_beetles = beetles,
         visit_lepidoptera = lepidoptera,
         visit_nonbee_hymenoptera = non_bee_hymenoptera,
         visit_others = other)

# Format Field_ID: from factor to number
visitation_rates_final$site_id <- as.character(visitation_rates_final$site_id)
visitation_rates_final$site_id <- parse_number(visitation_rates_final$site_id)

# Merge with data.site
data.site <- data.site %>% left_join(visitation_rates_final, by="site_id")
##############################################
##############################################
##############################################


# 5) Bartomeus_pollinator_richness: Archivo para insect sampling
# (el largo del transecto esta al inicio (50 metros, tiempo 15 minutos),
# no sabemos el numero de flores).
#Species (or morphospecies -> Gen sp) collected in 15 winter Oilseed rape sites (edge and middle of the crop -> Treatment) along a 50 m. - 15 minutes transect walk.


richness <- read.table("Bartomeus_pollinator_richness.txt", header = TRUE)
richness <- as_tibble(richness)
head(richness)

# Format Field: from factor to number
richness$Field <- as.character(richness$Field)
richness$Field <- parse_number(richness$Field)

richness %>% count(Family)

# Change Field by FieldID
#richness <- rename(richness, FieldID = Field)


gild_list <- c("Andrenidae" = "wild_bees","Anthophoridae" = "wild_bees",
               "Syrphidae" = "syrphids","Agromyzidae" = "other_flies",
               "Anthomyiidae" = "non_bee_hymenoptera",
               "Bibionidae" = "other_flies","Calliphoridae" = "other_flies",
               "Dolichopodidae" = "other_flies",
               "Elateridae" = "beetles","Empididae" = "other_flies",
               "Ichneumonidae" = "non_bee_hymenoptera",
               "Muscidae" = "other_flies","Rhagionidae" = "other_flies",
               "Sarcophagidae" = "other_flies",
               "Scathophagidae" = "other_flies","Stratiomydae" = "other_flies",
               "Tachinidae" = "other_flies",
               "Thenthredinindae" = "non_bee_hymenoptera")


richness <- arrange(richness,Field)
richness <- richness %>% mutate(Guild = unname(gild_list[as.character(richness$Family)]))


#There are some NA's that correspond to bombus and pieris gen

richness_NA <- richness %>% filter(is.na(Family))
NA_bombus <- is.na(richness$Family) & richness$Gen == "Bombus"
richness$Guild[NA_bombus] <- "bumblebees"

NA_pieris <- is.na(richness$Family) & richness$Gen == "Pieris"
richness$Guild[NA_pieris] <- "lepidoptera"

richness %>% group_by(Field,Treatment)%>% count()


# There are no control tratments
#richness %>% group_by(Treatment) %>% count() 


m_richness <- richness %>% mutate(Organism=paste(Gen,sp,sep=" ")) %>%
  group_by(Field,Treatment,Organism) %>%
  count(Guild)


########################################################
# GUILD ABUNDANCE
########################################################

m_richness2 <- m_richness %>% group_by(Field,Organism) %>%
  count(Guild,wt = n)

head(m_richness2)

#m_richness3 <- m_richness %>% group_by(Field) %>%
#  count(Guild,wt = n)

#head(m_richness3)

richness_field <- spread(m_richness2, key = Guild, value = n)

# NAs to zero

richness_field[is.na(richness_field)] <- 0
richness_field$honeybees <- NA
richness_field$humbleflies <- NA
richness_field$other <- NA

richness_field$total <- rowSums(richness_field[,3:12],na.rm=TRUE)

head(richness_field)

guild_abundance <- richness_field %>% ungroup() %>% select(-Organism) %>% group_by(Field)%>%
  summarise_all(sum, na.rm = TRUE) %>%
  rename(site_id = Field,
    abundance = total,
    ab_honeybee = honeybees,
    ab_bombus = bumblebees,
    ab_wildbees = wild_bees,
    ab_syrphids = syrphids,
    ab_humbleflies=humbleflies,
    ab_other_flies=other_flies,
    ab_beetles = beetles,
    ab_lepidoptera=lepidoptera,
    ab_nonbee_hymenoptera=non_bee_hymenoptera,
    ab_others = other
  )

# Merge abundance with data.site
data.site <- data.site %>% left_join(guild_abundance, by= "site_id")


###################################
# ESTIMATION RICHNESS INDEX
###################################
richness_field2 <- m_richness2 %>% select(-Guild) %>%
  spread(key = Organism, value = n)

richness_field2[is.na(richness_field2)] <- 0
richness_field2$r_obser <-  0
richness_field2$r_chao <-  0

for (i in 1:nrow(richness_field2)) {
  x <- as.numeric(richness_field2[i,2:(ncol(richness_field2)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  richness_field2$r_obser[i] <-  chao$Observed
  richness_field2$r_chao[i] <-  chao$Estimator 
}


richness_aux <- richness_field2 %>% select(site_id=Field,r_obser,r_chao)
richness_aux <- richness_aux %>% rename(observed_pollinator_richness=r_obser,
                                        other_pollinator_richness=r_chao) %>%
  mutate(other_richness_estimator_method="Chao1")

data.site <- data.site %>% left_join(richness_aux, by = "site_id")

###############################
# INSECT SAMPLING
###############################

insect_sampling <- tibble(
  study_id = "Ignasi_Bartomeus_Brassica_napus_Sweden_2013",
  site_id = m_richness2$Field,
  pollinator = m_richness2$Organism,
  guild = m_richness2$Guild,
  sampling_method = "Transect",
  abundance = m_richness2$n,
  total_sampled_area = 2*50,
  total_sampled_time = 2*15,
  total_sampled_flowers = NA,
  Description = "2 transects (50 m, 15 min. each)"
)

# To create insect sampling now we should add our plant observations.
# Format Field: from factor to number
m_visits_p$Site <- as.character(m_visits_p$Field)
m_visits_p$Site <- parse_number(m_visits_p$Site)

m_visits_p_sin_t <- m_visits_p %>% filter(!is.na(Guild))
m_visits_p_sin_t <- m_visits_p_sin_t %>% filter(counts!=0)

insect_sampling_visits <- tibble(
  study_id = "Ignasi_Bartomeus_Brassica_napus_Sweden_2013",
  site_id = m_visits_p_sin_t$Site,
  pollinator = m_visits_p_sin_t$Organism_ID,
  guild = m_visits_p_sin_t$Guild,
  sampling_method = "Plant observations",
  abundance = m_visits_p_sin_t$counts,
  total_sampled_area = 2*3*0.5,
  total_sampled_time = 2*3*5,
  total_sampled_flowers = m_visits_p_sin_t$flowers,
  Description = "2 rounds (3 sites: 0.5m2, 5 min. each)"
)

insect_sampling_total <- insect_sampling %>% bind_rows(insect_sampling_visits)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling_total, "insect_sampling_Ignasi_Bartomeus_Brassica_napus_Sweden_2013.csv")

setwd(dir_ini)


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
  field_size = NA,
  yield=data.site$yield,
  yield_units=data.site$yield_units,
  yield2=NA,
  yield2_units=NA,
  yield_treatments_no_pollinators=NA,
  yield_treatments_pollen_supplement=NA,
  yield_treatments_no_pollinators2=NA,
  yield_treatments_pollen_supplement2=NA,
  fruits_per_plant=data.site$fruits_per_plant,
  fruit_weight= NA,
  plant_density=data.site$plant_density,
  seeds_per_fruit=data.site$seeds_per_fruit,
  seeds_per_plant=data.site$seeds_per_plant,
  seed_weight=data.site$seed_weight,
  observed_pollinator_richness=data.site$observed_pollinator_richness,
  other_pollinator_richness=data.site$other_pollinator_richness,
  other_richness_estimator_method=data.site$other_richness_estimator_method,
  abundance = data.site$abundance,
  ab_honeybee = data.site$ab_honeybee,
  ab_bombus = data.site$ab_bombus,
  ab_wildbees = data.site$ab_wildbees,
  ab_syrphids = data.site$ab_syrphids,
  ab_humbleflies=data.site$ab_humbleflies,
  ab_other_flies=data.site$ab_other_flies,
  ab_beetles=data.site$ab_beetles,
  ab_lepidoptera=data.site$ab_lepidoptera,
  ab_nonbee_hymenoptera=data.site$ab_nonbee_hymenoptera,
  ab_others = data.site$ab_others,
  total_sampled_area = 2*50,
  total_sampled_time = 2*15,
  visitation_rate_units = "visits per 100 flowers and hour",
  visitation_rate = data.site$visitation_rate,
  visit_honeybee = data.site$visit_honeybee,
  visit_bombus = data.site$visit_bombus,
  visit_wildbees = data.site$visit_wildbees,
  visit_syrphids = data.site$visit_syrphids,
  visit_humbleflies = data.site$visit_humbleflies,
  visit_other_flies = data.site$visit_other_flies,
  visit_beetles = data.site$visit_beetles,
  visit_lepidoptera = data.site$visit_lepidoptera,
  visit_nonbee_hymenoptera = data.site$visit_nonbee_hymenoptera,
  visit_others = data.site$visit_others,
  Publication = data.site$Publication,
  Credit = data.site$Credit,
  Email_contact = data.site$Email_contact
)

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(field_level_data, "field_level_data_Ignasi_Bartomeus_Brassica_napus_Sweden_2013.csv")
setwd(dir_ini)
