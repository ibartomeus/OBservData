
# load libraries
library(tidyverse)
library(openxlsx)

dir_ini <- getwd()

# Load data
dir <- "Processing_files/Datasets_Processing/Tim_Weekers_CliPS/"
file <- paste0(dir,"Observ_TW_new.xlsx")

#Load datasets: field_data
data.site <- read.xlsx(file,
                       sheet = "field_level_data", startRow = 1)
data.site <- as_tibble(data.site)


# Adapting raw info to CropPol's new template
data.site$country %>% unique()
data.site$study_id[data.site$country=="France"] <- "Timothy_Weekers_Malus_domestica_France_2019"
data.site$study_id[data.site$country=="Spain"] <- "Timothy_Weekers_Malus_domestica_Spain_2019"
data.site$study_id[data.site$country=="Netherlands"] <- "Timothy_Weekers_Malus_domestica_Netherlands_2019"
data.site$study_id[data.site$country=="Morocco"] <- "Timothy_Weekers_Malus_domestica_Morocco_2019"
data.site$study_id[data.site$country=="United Kingdom"] <- "Timothy_Weekers_Malus_domestica_UK_2019"

data.site$country[data.site$country=="United Kingdom"] <- "UK"

data.site$management %>% unique()
data.site$management[data.site$management=="Organic"] <- "organic"
data.site$management[data.site$management=="Non-organic"] <- "IPM"
data.site$management %>% unique()


data.site$sampling_end_month %>% unique()
data.site$sampling_start_month %>% unique()
data.site$sampling_end_month[data.site$sampling_end_month=="April"] <- 4
data.site$sampling_start_month[data.site$sampling_start_month=="April"] <- 4

data.site$total_sampled_time %>% unique()
data.site$total_sampled_time <- 60*as.numeric(gsub("[^[:digit:]]", "", data.site$total_sampled_time))


########################
# Field level data


field_level_data <- tibble(
  study_id = data.site$study_id,
  site_id = data.site$site_id,
  crop = data.site$crop,
  variety = data.site$variety,
  management = data.site$management,
  country = data.site$country,
  latitude = data.site$latitude,
  longitude = data.site$longitude,
  X_UTM = NA,
  Y_UTM = NA,
  zone_UTM = NA,
  sampling_start_month = data.site$sampling_start_month,
  sampling_end_month = data.site$sampling_end_month,
  sampling_year = data.site$sampling_year,
  field_size = data.site$field.size,
  yield = data.site$total_yield,
  yield_units = "kg per ha",
  yield2 = NA,
  yield2_units = NA,
  yield_treatments_no_pollinators = NA,
  yield_treatments_pollen_supplement = NA,
  yield_treatments_no_pollinators2 = NA,
  yield_treatments_pollen_supplement2 = NA,
  fruits_per_plant = NA,
  fruit_weight = NA,
  plant_density = NA,
  seeds_per_fruit = NA,
  seeds_per_plant = NA,
  seed_weight = NA,
  sampling_richness = NA,
  observed_pollinator_richness = NA,
  other_pollinator_richness = NA,
  other_richness_estimator_method = NA,
  richness_restriction = NA,
  sampling_abundance = NA,
  abundance = NA,
  ab_honeybee = NA,
  ab_bombus = NA,
  ab_wildbees = NA,
  ab_syrphids = NA,
  ab_humbleflies = NA,
  ab_other_flies = NA,
  ab_beetles = NA,
  ab_lepidoptera = NA,
  ab_nonbee_hymenoptera = NA,
  ab_others = NA,
  total_sampled_area = 10000 * data.site$total_sampled_area,
  total_sampled_time = data.site$total_sampled_time,
  sampling_visitation = NA,
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
  Publication = "10.1016/j.agee.2021.107697",
  Credit = "Timothy Weekers, Jordi Bosch, Diego Cejas, Bianca Drepper, Michael Garratt, Louise Hutchinson, Nicolas Leclercq, Patrick Lhomme, Leon Marshall, Denis Michez, Jean-Marc Molenberg, Stuart Roberts, Laura Roquer-Beni, Guy Smagghe, Peter Vandamme, Nicolas J. Vereecken, Thomas J. Wood",
  Email_contact = "Timothy.weekers@ulb.be, Nicolas.Vereecken@ulb.be",
  notes = NA
)


write_csv(field_level_data,paste0(dir,
                           "field_level_data_Timothy_Weekers_Malus_domestica_CliPS.csv"))


######################################
# Insect sampling

#Load datasets: insect_sampling
data.insect <- read.xlsx(file,
                         sheet = "insect_sampling", startRow = 1)
data.insect <- as_tibble(data.insect)

study_site_IDs <- field_level_data[,c("study_id","site_id")]


data.insect <- data.insect %>% rename(study_id_old = study_id)  %>%
  left_join(study_site_IDs, by = c("site_id")) %>% select(-study_id_old)

data.insect$study_id %>% unique()

data.insect$Total_sampled_time %>% unique()
data.insect$Total_sampled_time <- 60*as.numeric(gsub("[^[:digit:]]", "", data.insect$Total_sampled_time))

data.insect$Total_sampled_area %>% unique()

data.insect$Guild %>% unique()
data.insect$Guild[data.insect$Guild=="other wild bees"] <- "other_wild_bees"

data.insect$Pollinator[grep("agg",data.insect$Pollinator)] %>% unique()
data.insect$identified_to <- "species"
data.insect$identified_to[grep("agg",data.insect$Pollinator)] <- "group of both Bombus terrestris and Bombus lucorum specimens"
data.insect$identified_to %>% unique()

insect_sampling <- tibble(
  study_id = data.insect$study_id,
  site_id = data.insect$site_id,
  sampling_method = data.insect$Sampling.method,
  pollinator = data.insect$Pollinator,
  identified_to = data.insect$identified_to,
  guild = data.insect$Guild,
  abundance = data.insect$Abundance,
  total_sampled_area = 10000 * data.insect$Total_sampled_area,
  total_sampled_time = data.insect$Total_sampled_time,
  total_sampled_flowers = data.insect$Total_sampled_flowers,
  description = "Each site was sampled for three consecutive days during the peak blooming period, using a standardized protocol combining active (netting) and passive (pan traps) collection methods. Each day, active sampling from apple blossoms allowed surveying the bee community directly associated with the crop, starting at a random point and then following the orchard lanes during two periods of 90 minutes. The passive sampling was deployed on cleared ground using trios of painted pan traps (fluorescent blue, fluorescent yellow, white), (i.e., nine pan traps in total with three pan traps of each color, set for the day and collected late afternoon) filled with soapy water.",
  notes = ""
)

write_csv(insect_sampling,paste0(dir,
                           "insect_sampling_Timothy_Weekers_Malus_domestica_CliPS.csv"))


#############################
# Sampling method

data.sampling <- insect_sampling %>% group_by(study_id,sampling_method) %>%
  count() %>% select(-n)

data.sampling$sampling_method %>% unique()
data.sampling$type <- "sweep net"
data.sampling$type[data.sampling$sampling_method != "Net"] <- "pan trap, bee bowl, blue vane trap, pitfall"
data.sampling$sampling_richness <- 1
data.sampling$sampling_richness[data.sampling$sampling_method != "Net"] <- 0

data.sampling$sampling_abundance <- 1
data.sampling$sampling_abundance[data.sampling$sampling_method != "Net"] <- 0


sampling_method <- tibble(
  study_id = data.sampling$study_id,
  sampling_method = data.sampling$sampling_method,
  type = data.sampling$type,
  sampling_richness = data.sampling$sampling_richness,
  sampling_abundance = data.sampling$sampling_abundance,
  notes = NA
)

write_csv(sampling_method,paste0(dir,
                                 "sampling_method_Timothy_Weekers_Malus_domestica_CliPS.csv"))


#######################################
# Ownership


ownership_aux <- tibble(
  study_id = NA,
  name = c("Timothy Weekers",
           "Jordi Bosch",
           "Diego Cejas",
           "Bianca Drepper",
           "Michael Garratt",
           "Louise Hutchinson",
           "Nicolas Leclercq",
           "Patrick Lhomme",
           "Leon Marshall",
           "Denis Michez",
           "Jean-Marc Molenberg",
           "Stuart Roberts",
           "Laura Roquer-Beni",
           "Guy Smagghe",
           "Peter Vandamme",
           "Nicolas J. Vereecken",
           "Thomas J. Wood"),
  affiliation = c(
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Centre de Recerca Ecologica i Aplicacions Forestals, Universitat Autonoma de Barcelona, 08193, Spain",
    "Laboratory of Zoology, Research Institute for Biosciences, University of Mons, B-7000 Mons, Belgium",
    "Division of Forest, Nature and Landscape, University of Leuven, Leuven, B-3001 Belgium",
    "School of Agriculture, Policy and Development, University of Reading, Reading, RG6 6EU United Kingdom",
    "School of Agriculture, Policy and Development, University of Reading, Reading, RG6 6EU United Kingdom",
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Laboratory of Zoology, Research Institute for Biosciences, University of Mons, B-7000 Mons, Belgium",
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Laboratory of Zoology, Research Institute for Biosciences, University of Mons, B-7000 Mons, Belgium",
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Centre de Recerca Ecologica i Aplicacions Forestals, Universitat Autonoma de Barcelona, 08193, Spain",
    "Department of plants and crops, Ghent University, Ghent, B-9000 Belgium",
    "Department of biochemistry and microbiology, Ghent University, Ghent, B-9000 Belgium",
    "Agroecology Lab, Universite libre de Bruxelles, Brussels, B-1050 Belgium",
    "Laboratory of Zoology, Research Institute for Biosciences, University of Mons, B-7000 Mons, Belgium"
  ),
  email = c("Timothy.Weekers@ulb.be",
            "Jordi.Bosch@uab.cat",
            "diegomanuel.cejasacuna@umons.ac.be",
            "bianca.drepper@kuleuven.be",
            "m.p.garratt@reading.ac.uk",
            "L.Hutchinson@pgr.reading.ac.uk",
            "nicolas.leclercq@ulb.be",
            "patrick_lhomme@hotmail.fr",
            "Leon.Marshall@ulb.be",
            "Denis.MICHEZ@umons.ac.be",
            "Jean.Marc.Molenberg@ulb.be",
            "spmr@msn.com",
            "lauraroquerbeni@gmail.com",
            "Guy.Smagghe@ugent.be",
            "Peter.Vandamme@ugent.be",
            "Nicolas.Vereecken@ulb.be",
            "ThomasJames.WOOD@umons.ac.be"
  ),
  role = c("Lead author/Corresponding author",rep("Co-author/Co-owner",16)),
  funding = NA
)

list_studies <- field_level_data$study_id %>% unique()

ownership <- NULL

for(i in list_studies){

  ownership_i <- ownership_aux
  ownership_i$study_id <- i
  ownership <- bind_rows(ownership,ownership_i)
}

write_csv(ownership,paste0(dir,"ownership_Timothy_Weekers_Malus_domestica_CliPS.csv"))

