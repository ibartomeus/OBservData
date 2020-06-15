
# This script updates field_level_data files extracted from Rader and Dainese.
# Specifically, it includes the column "richness_restriction". 
# Files in Kleijn (2015) already include such information.

library(tidyverse)


folder_base <- "../Datasets_storage"

files_base <- list.files(folder_base)


# List of files (in our data repository folder) whose name begins with
# "field_level_data"

list_files_field_level <- files_base[grepl("field_level_data", files_base)]

# extract_template_i: function that merges the dataset files
# fiel_level_ data: file that contains the whole dataset

extract_template_i <- function(file_name){
  field_level_i <- read_csv(file_name,
                            col_types = cols(
                                       study_id = col_character(),
                                       site_id = col_character(),
                                       crop = col_character(),
                                       variety = col_character(),management = col_character(),
                                       country = col_character(),latitude = col_double(),
                                       longitude = col_double(),X_UTM = col_double(),
                                       Y_UTM = col_double(),zone_UTM = col_character(),
                                       sampling_start_month = col_double(),
                                       sampling_end_month = col_double(),sampling_year = col_character(),
                                       field_size = col_double(),
                                       yield = col_double(),
                                       yield_units = col_character(),
                                       yield2 = col_double(),yield2_units = col_character(),
                                       yield_treatments_no_pollinators = col_double(),
                                       yield_treatments_pollen_supplement = col_double(),
                                       yield_treatments_no_pollinators2 = col_double(),
                                       yield_treatments_pollen_supplement2 = col_double(),
                                       fruits_per_plant = col_double(),fruit_weight = col_double(),
                                       plant_density = col_double(),seeds_per_fruit = col_double(),
                                       seeds_per_plant = col_double(),seed_weight = col_double(),
                                       observed_pollinator_richness = col_double(),
                                       other_pollinator_richness = col_double(),
                                       other_richness_estimator_method = col_character(),
                                       abundance = col_double(),ab_honeybee = col_double(),
                                       ab_bombus = col_double(),ab_wildbees = col_double(),
                                       ab_syrphids = col_double(),ab_humbleflies = col_double(),
                                       ab_other_flies = col_double(),ab_beetles = col_double(),
                                       ab_lepidoptera = col_double(),ab_nonbee_hymenoptera = col_double(),
                                       ab_others = col_double(),total_sampled_area = col_double(),
                                       total_sampled_time = col_double(),
                                       visitation_rate_units = col_character(),
                                       visitation_rate = col_double(),visit_honeybee = col_double(),
                                       visit_bombus = col_double(),visit_wildbees = col_double(),
                                       visit_syrphids = col_double(),visit_humbleflies = col_double(),
                                       visit_other_flies = col_double(),visit_beetles = col_double(),
                                       visit_lepidoptera = col_double(),visit_nonbee_hymenoptera = col_double(),
                                       visit_others = col_double(),
                                       Publication = col_character(),
                                       Credit = col_character(),Email_contact = col_character()))
  field_level_i
}


create_file <-  TRUE

for (i in seq(length(list_files_field_level))) {
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  field_level_i <- extract_template_i(file_field_level_i)
  
  if (!"richness_restriction" %in% colnames(field_level_i)){
    
    abundance_non_bee <- rowSums(field_level_i[,37:43],na.rm = TRUE)
    visit_non_bee <- rowSums(field_level_i[,51:57],na.rm = TRUE)
    
    result_i <- tibble(file=list_files_field_level[i],
                       abundance_non_bee=sum(abundance_non_bee),
                       visits_non_bee=sum(visit_non_bee))
    
    if (create_file == TRUE){
      
      create_file <- FALSE
      
      results <- result_i
    }else{
      results <- bind_rows(results,result_i)
    }
    
    
  }
  
}
results_filtered <- results %>% filter(abundance_non_bee+visits_non_bee==0)

results_filtered$richness_restriction[results_filtered$file=="field_level_data_Georg_Andersson_Fragaria_ananassa_Sweden_2009.csv"] <- "bees+hoverflies"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Virginie_Boreux_Coffea_canephora_India_2008.csv"] <- "all visitors considered. Ended up with bees only (mainly social)"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Luísa_G_Carvalheiro_Helianthus_annuus_South_Africa_2009.csv"] <- "all visitors considered"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Johan_Ekroos_Vicia_faba_Sweden_2016.csv"] <- "only bumblebees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Breno_M_Freitas_Malpighia_emarginata_Brazil_2011.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Breno_M_Freitas_Bixa_orellana_Brazil_2007.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Breno_M_Freitas_Gossypium_hirsutum_Brazil_2011.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Heather_Lee_Grab_Fragaria_ananassa_USA_2012.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Heather_Lee_Grab_Fragaria_ananassa_USA_2014.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Heather_Lee_Grab_Fragaria_ananassa_USA_2015.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Violeta_Hevia_Helianthus_annuus_Spain_2017.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Jessica_Knapp_Cucurbita_pepo_UK_2016.csv"] <- "all visitors considered. Non-bee data was obtained by using pantraps"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Rachel_Mallinger_Malus_domestica_USA_2012.csv"] <- "only bees. Data was obtained by using pantraps"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Rachel_Mallinger_Malus_domestica_USA_2013.csv"] <- "only bees. Data was obtained by using pantraps"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Simon_Potts_Vicia_faba_UK_2005.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Agustin_Saez_Rubus_idaeus_Argentina_2014.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Nicolas_J_Vereecken_several_crops_several_countries_several_years.csv"] <- "only bees"
results_filtered$richness_restriction[results_filtered$file=="field_level_data_Yi_Zou_Brassica_napus_China_2015.csv"] <- "all visitors considered. Data was obtained by using pantraps"

####################################
# Update Files
####################################


for (i in seq(length(list_files_field_level))) {
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  data.site <- extract_template_i(file_field_level_i)
  
  if (!"richness_restriction" %in% colnames(data.site)){
    
    if (list_files_field_level[i] %in% results_filtered$file){
      aux_i <- results_filtered$richness_restriction[results_filtered$file==list_files_field_level[i]]
    }else{
      aux_i <- NA
    }
    
    field_level_data <- tibble(
      study_id = data.site$study_id,
      site_id = data.site$site_id,
      crop = data.site$crop,
      variety = data.site$variety,
      management = data.site$management,
      country = data.site$country,
      latitude = data.site$latitude,
      longitude = data.site$longitude,
      X_UTM=data.site$X_UTM,
      Y_UTM=data.site$Y_UTM,
      zone_UTM=data.site$zone_UTM,
      sampling_start_month = data.site$sampling_start_month,
      sampling_end_month = data.site$sampling_end_month,
      sampling_year = data.site$sampling_year,
      field_size = data.site$field_size,
      yield=data.site$yield,
      yield_units=data.site$yield_units,
      yield2=data.site$yield2,
      yield2_units=data.site$yield2_units,
      yield_treatments_no_pollinators=data.site$yield_treatments_no_pollinators,
      yield_treatments_pollen_supplement=data.site$ yield_treatments_pollen_supplement,
      yield_treatments_no_pollinators2=data.site$yield_treatments_no_pollinators2,
      yield_treatments_pollen_supplement2=data.site$yield_treatments_pollen_supplement2,
      fruits_per_plant=data.site$fruits_per_plant,
      fruit_weight= data.site$fruit_weight,
      plant_density=data.site$plant_density,
      seeds_per_fruit=data.site$seeds_per_fruit,
      seeds_per_plant=data.site$seeds_per_plant,
      seed_weight=data.site$seed_weight,
      observed_pollinator_richness=data.site$observed_pollinator_richness,
      other_pollinator_richness=data.site$other_pollinator_richness,
      other_richness_estimator_method=data.site$other_richness_estimator_method,
      richness_restriction = aux_i,
      abundance = data.site$abundance,
      ab_honeybee = data.site$ab_honeybee,
      ab_bombus = data.site$ab_bombus,
      ab_wildbees = data.site$ab_wildbees,
      ab_syrphids = data.site$ab_syrphids,
      ab_humbleflies= data.site$ab_humbleflies,
      ab_other_flies= data.site$ab_other_flies,
      ab_beetles=data.site$ab_beetles,
      ab_lepidoptera=data.site$ab_lepidoptera,
      ab_nonbee_hymenoptera=data.site$ab_nonbee_hymenoptera,
      ab_others = data.site$ab_others,
      total_sampled_area = data.site$total_sampled_area,
      total_sampled_time = data.site$total_sampled_time,
      visitation_rate_units = data.site$visitation_rate_units,
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
    
    # Uncomment to write file
    write_csv(field_level_data, file_field_level_i) # Commented for security reasons
    
  }
  
}

