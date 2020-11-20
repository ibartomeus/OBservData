
# This script groups the values of variable "sampling method" in the file
# CropPol_sampling_data.csv

library(tidyverse)

# Load data

final_insect_sampling <- 
  "CropPol_sampling_data.csv"

final_insect_sampling_data <- read.csv(final_insect_sampling,encoding = "WINDOWS-1252",stringsAsFactors = F)
final_insect_sampling_data <- as_tibble(final_insect_sampling_data)
#######################

sampling_data <- final_insect_sampling_data %>% select(study_id,sampling_method) %>% unique()

# Studies without sampling method
sampling_data %>% filter(is.na(sampling_method)) %>% select(study_id) %>% unique()
# 1 Sandra_Lindstrom_Brassica_napus_Sweden_2011
# 2 Sandra_Lindstrom_Brassica_napus_Sweden_2012

# Te reference paper indicates that recorbs were obtained from observations and netting along transects at distances of 100, 200 and 300 m, respectively, from the side of the field with the shortest width
# However, we dont know which method was used for each record.


sampling_data$sampling_method[grep("censuses of 15 minutes observation",
                                   sampling_data$sampling_method,
                                   ignore.case = T)] <- "observation"

sampling_data$sampling_method[grep("minutes observation to a flowering branch",
                                   sampling_data$sampling_method,
                                   ignore.case = T)] <- "observation"


sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("Ariel Netting from cotton flowers",
                                  "Net","netting","netting form flower",
                                  "netting/observation","insect collection",
                                  "sweep","sweep net","sweep net along a transect",
                                  "sweep net, note that flower abundance (hence sampling effort) varied between sites",
                                  "sweep_net","sweepnet","sweepnet","sweep netting",
                                  "flower visiting individuals only","Sweepnets"
                                  )] <- "Net, sweep net"


sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("obs","observation","observations",
                                  "Plant observation","plant observations",
                                  "Plant observations",
                                  "Direct ocular observations",
                                  "5 minute focal observation",
                                  "observations (15min/observation, simulatenous)",
                                  "census","Census of 5 minutes")] <- "observation"

sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("pan-traps","Pan-traps",
                                  "pan trap","Pantrap","pantraps",
                                  "bee_bowl","blue_vane","blue_vane_trap",
                                  "pitfall","Plan traps")] <- "pan trap, bee bowl, blue vane trap, pitfall"

sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("sample of female flowers",
                                  "samples of flower visitors",
                                  "individual collection")] <- "samples of flower visitors"

# survey means: standardized transect walks with an aerial net (Riccardo_Bommarco_Brassica_napus_Sweden_2005)
sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("Standardized transect walks","survey",
                                  "transect","Transect",
                                  "transect counts","transect observations",
                                  "transects (in each transect I observed the number and identity of pollinatior visiting 200 flowers).",
                                  "transects",
                                  "30min Observation/site (3x2 transects). a 5min transect (20m) was walked between 2 apple tree rows")] <- "transect"
# Consulta a Nacho
# A) At each site, a portion of a coffee plant composed ofapproximately 250 flowers 
# was selected, and bee activityat these flowers was observed for 10 minutes 
# (Kearns &Inouye 1993). Every visitor and the number of flowers itvisited were recorded. 
# (I define a “visit” as a bee landingon a flower and collecting resources from it.) 
# For eachsite, two such 10-minute observations were conductedsimultaneously on different
# plants and the counts pooledforanalysis.
#
# B) At peak flowering time in No-vember 2001, between 0900 and 1300 hours, we col-lected
# a single sample of 100 female-phase flowers from 30 to 50 randomly selected trees of the 
# PinksMammoth variety at each orchard. The flowers were placed in plastic bags and frozen
# to kill any insects present. The samples were later thawed and the insects extracted, 
# sorted to mor-phospecies, and their abundance at each orchard re-corded.

# C) As many bees as possible were cap-tured with either an insect net or jar as the 
# indi-vidual collecting moved throughout the bog for 15min in 1990 and for 10 min in 1991.
# All bees, in-cluding honey bees, foraging on cranbeny bloomwere collected.

sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("sample of female flowers",
                                  "samples of flower visitors")] <- "other"

sampling_data$sampling_method[sampling_data$sampling_method %in%
                                c("individual collection")] <- "Net, sweep net"


sampling_data$sampling_method %>% unique()

