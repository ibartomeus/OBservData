
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 87_TaylorRickettsCoffee_2001
##########################

data_raw <- read_excel("87_88_TaylorRickettsCoffee_2001_2002/field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001_UPDATED.xlsx",
                       sheet = "field_level_data_Taylor_Rickett")
data_raw <- as_tibble(data_raw)


# Add latitude and longitude
latlon <- data_raw %>% select(`LAT LON TOGETHER (DECIMAL DEG)`) %>%
  separate(`LAT LON TOGETHER (DECIMAL DEG)`,c("latitude","longitude"),", ")

data_raw <- data_raw %>% rename(latitude=`LAT LON TOGETHER (DECIMAL DEG)`)
data_raw$latitude <- latlon$latitude
data_raw$longitude <- latlon$longitude
  
# Add DOI
data_raw$Publication <- "10.1073/pnas.0405147101,10.1111/j.1523-1739.2004.00227.x"

# Add yield

# hand-pollination (receiving augmented pollen to measure
# production with abundant cross-pollination)
# and control (unmanipulated to measure production under ambient pollination) 

yield_raw <- 
read_excel("87_88_TaylorRickettsCoffee_2001_2002/Ricketts yield data for OBServ.xls",
           sheet = "data")

seed_mass <- yield_raw %>% select(-`fruit set`) %>% 
  spread(key = Treatment, value = `seed mass`)

fruit_set <- yield_raw %>% select(-`seed mass`) %>% 
  spread(key = Treatment, value = `fruit set`)

data_raw$yield <- fruit_set$open
data_raw$yield_units <- "fruit set (%)"
data_raw$yield_treatments_pollen_supplement <- fruit_set$hand

data_raw$yield2 <- seed_mass$open
data_raw$yield2_units <- "wet seed mass per fruit"
data_raw$yield_treatments_pollen_supplement2 <- seed_mass$hand


# Abundance and visitation

abundance_visits <- 
  read_excel("87_88_TaylorRickettsCoffee_2001_2002/Ricketts_data_for_Pasha.xls",
             sheet = "AttribsFincasites") %>% filter(YEAR==2001)

data_raw$observed_pollinator_richness <- abundance_visits$Richness
data_raw$abundance <- abundance_visits$Abundance
data_raw$ab_wildbees <- abundance_visits$Abund_natives
data_raw$ab_honeybee <- data_raw$abundance-data_raw$ab_wildbees


# Visits are given in the datasheet as "visits per 100 flowers and 20 min"
data_raw$visitation_rate <- (3)*abundance_visits$`all bees visitation rate`
data_raw$visitation_rate_units <- "visits per 100 flowers and hour"
data_raw$visit_honeybee <- (3)*(
  abundance_visits$`all bees visitation rate`-abundance_visits$`native visitation rate`)
data_raw$visit_wildbees <- (3)*abundance_visits$`native visitation rate`

# Sampling effort

data_raw$total_sampled_time <- 10*abundance_visits$SAMPLES

###############################
# FIELD LEVEL DATA
###############################

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data_raw, "field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
setwd(dir_ini)


###########################
# INSECT SAMPLING
###########################

insect_sampling_aux <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage/insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")

sampling <- abundance_visits %>% select(FINCASITES,SAMPLES) %>% rename(site_id=FINCASITES)

insect_sampling <- insect_sampling_aux %>% 
  left_join(sampling,by="site_id") %>%
  mutate(total_sampled_time=10*SAMPLES,total_sampled_flowers=250*SAMPLES) %>%
  select(-SAMPLES)


setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2001.csv")
setwd(dir_ini)
