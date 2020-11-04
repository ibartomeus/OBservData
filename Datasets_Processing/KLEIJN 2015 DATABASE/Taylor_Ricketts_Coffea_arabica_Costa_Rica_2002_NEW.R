
library(tidyverse)
library(sp) #Transforming latitude and longitude
library("iNEXT")
library(openxlsx)
library(readxl)
library(parzer) #parse coordinates

dir_ini <- getwd()

##########################
#Data: 87_TaylorRickettsCoffee_2002
##########################

# Abundance and visitation

abundance_visits <- 
  read_excel("87_88_TaylorRickettsCoffee_2001_2002/Ricketts_data_for_Pasha.xls",
             sheet = "AttribsFincasites") %>% filter(YEAR==2002)

###########################
# INSECT SAMPLING
###########################

insect_sampling_aux <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage/insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2002.csv")

sampling <- abundance_visits %>% select(FINCASITES,SAMPLES) %>% rename(site_id=FINCASITES)

insect_sampling <- insect_sampling_aux %>% 
  left_join(sampling,by="site_id") %>%
  mutate(total_sampled_time=10*SAMPLES,total_sampled_flowers=250*SAMPLES) %>%
  select(-SAMPLES)

# Add guild to Huge black 2002
insect_sampling %>% filter(is.na(guild)) %>% select(pollinator)
insect_sampling$guild[insect_sampling$pollinator=="Huge Black 2002"] <- "other_wild_bees"

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(insect_sampling, "insect_sampling_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2002.csv")
setwd(dir_ini)

######################
# FIELD LEVEL INFO
######################

data_raw <- read_excel("87_88_TaylorRickettsCoffee_2001_2002/field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2002_UPDATED.xlsx",
                       sheet = "field_level_data_Taylor_Rickett")
data_raw <- as_tibble(data_raw)

  
# Add DOI
data_raw$Publication <- "10.1073/pnas.0405147101,10.1111/j.1523-1739.2004.00227.x"


# Abundance and visitation

abundance_visits <- 
  read_excel("87_88_TaylorRickettsCoffee_2001_2002/Ricketts_data_for_Pasha.xls",
             sheet = "AttribsFincasites") %>% filter(YEAR==2002)


data_raw$observed_pollinator_richness[5:16] <- abundance_visits$Richness
data_raw$abundance[5:16] <- abundance_visits$Abundance
data_raw$ab_wildbees[5:16] <- abundance_visits$Abund_natives
data_raw$ab_honeybee[5:16] <- abundance_visits$Abundance-abundance_visits$Abund_natives

data_raw$abundance[1:4] <- data_raw$visitation_rate[1:4]/6
data_raw$ab_wildbees[1:4] <- data_raw$visit_wildbees[1:4]/6
data_raw$ab_honeybee[1:4] <- data_raw$visit_honeybee[1:4]/6


data_raw$visitation_rate <- NA
data_raw$visitation_rate_units <- NA
data_raw$visit_wildbees <- NA
data_raw$visit_honeybee <- NA

# Visits are given in the datasheet as "visits per 100 flowers and 20 min"

data_raw$visitation_rate[5:16] <- (3)*abundance_visits$`all bees visitation rate`
data_raw$visitation_rate_units[5:16] <- "visits per 100 flowers and hour"
data_raw$visit_honeybee[5:16] <- (3)*(
  abundance_visits$`all bees visitation rate`-abundance_visits$`native visitation rate`)
data_raw$visit_wildbees[5:16] <- (3)*abundance_visits$`native visitation rate`

# Sampling effort

data_raw$total_sampled_time[5:16] <- 10*abundance_visits$SAMPLES

###############################
# FIELD LEVEL DATA
###############################

setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")
write_csv(data_raw, "field_level_data_Taylor_Ricketts_Coffea_arabica_Costa_Rica_2002.csv")
setwd(dir_ini)
