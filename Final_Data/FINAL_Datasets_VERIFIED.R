
# Merge individual "field_level_data" and "insect_sampling_data"  files, respectively,
# if the corresponding files tests are OK and if the files have been checked by
# their corresponding authors.

library(tidyverse)
library(testthat)


folder_base <- "../Datasets_storage"

files_base <- list.files(folder_base)

#Date: 27/10/2020 -> Version: 0.2

##############################
# VERIFIED DATASETS
##############################

verified <- openxlsx:::read.xlsx("C:/Users/USUARIO/Desktop/Folders for authors/FINAL_Data ownership.xlsx")
verified_studies <- verified %>% select(study_id) %>% unique()
  
##############################
# MERGE FIELD_LEVEL DATA
##############################


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
                              sampling_end_month = col_double(),
                              field_size = col_double(),sampling_year = col_character(),
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
                              richness_restriction = col_character(),
                              abundance = col_double(),ab_honeybee = col_double(),
                              ab_bombus = col_double(),ab_wildbees = col_double(),
                              ab_syrphids = col_double(),ab_humbleflies = col_double(),
                              ab_other_flies = col_double(),ab_beetles = col_double(),
                              ab_lepidoptera = col_double(),ab_nonbee_hymenoptera = col_double(),
                              ab_others = col_double(),
                              total_sampled_area = col_character(),
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

#Test data files and save the results

test_file("../testthat/test-format-field_level.R", reporter = "summary") #Visualize testing
options(testthat.output_file = "../testthat/test_out_Create_Dataset.txt")
# test_file("../testthat/test-format.R", reporter = "summary")

# Extract the names of the files which contain failures

report <- readLines("../testthat/test_out_Create_Dataset.txt")

file.failures <- str_match(report, "field_level_data_(.*?)csv")
file.failures <- file.failures[!is.na(file.failures[,1]),1]
file.failures <- file.failures[!duplicated(file.failures)]

# Merge the files without failures

list_files_field_level <- list_files_field_level[!list_files_field_level %in% file.failures]

for (i in seq(length(list_files_field_level))) {
  
  file_field_level_i <- paste(folder_base, list_files_field_level[i], sep = "/")
  field_level_i <- extract_template_i(file_field_level_i)
  
  if (i == 1) {
    FINAL_field_level_data <- field_level_i
    }
  else {
    FINAL_field_level_data <- FINAL_field_level_data %>% bind_rows(field_level_i)
    }
  
}

# Fix crop names:


FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Malus Domestica"] <- "Malus domestica"
FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Fragaria × ananassa"] <- "Fragaria x ananassa"

# Fix country:
FINAL_field_level_data$country[FINAL_field_level_data$country=="United States"] <- "USA"
FINAL_field_level_data$country %>% unique() %>% sort()

# Fix variety:
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Koipesol Oleko"] <- "Koipesol OLEKO"
FINAL_field_level_data$variety %>% unique() %>% sort()

# Fix pollinator restriction

FINAL_field_level_data$richness_restriction[is.na(FINAL_field_level_data$richness_restriction)] <- "none"
FINAL_field_level_data$richness_restriction[is.na(FINAL_field_level_data$observed_pollinator_richness)&
                                              is.na(FINAL_field_level_data$other_pollinator_richness)&
                                              is.na(FINAL_field_level_data$other_richness_estimator_method)] <- NA



FINAL_field_level_data$richness_restriction %>% unique() %>% sort()
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="Only bees"] <- "only bees"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="none"] <- "all visitors considered"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="bees+hoverflies"] <- "bees and hoverflies"

FINAL_field_level_data$richness_restriction %>% unique() %>% sort()

#Fix other_richness_estimator_method

FINAL_field_level_data$other_richness_estimator_method[FINAL_field_level_data$other_richness_estimator_method=="Chao1"] <- "Chao 1"


# Fix Publication
FINAL_field_level_data$Publication %>% unique()

FINAL_field_level_data$Publication[FINAL_field_level_data$Publication==":10.1016/j.agee.2008.08.001"] <- "10.1016/j.agee.2008.08.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="https://doi.org/10.1038/s41598-019-49535-w; yield data unpublished"] <- "10.1038/s41598-019-49535-w; yield data unpublished"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="Trillo, A., Herrera, J. M., & Vilà, M. (2018). Managed bumble bees increase flower visitation but not fruit weight in polytunnel strawberry crops. Basic and Applied Ecology, 30, 32-40."] <- "10.1016/j.baae.2018.05.008"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="none - writing in progress"] <- "In preparation"


####################
# Select verified studies

FINAL_field_level_data_filt <- FINAL_field_level_data %>% filter(study_id %in%
                                  verified_studies$study_id)

FINAL_field_level_data_filt$Credit[grep("Christof Sch",FINAL_field_level_data_filt$Credit)] <- "Christof Schüepps, Felix Herzog and Martin H. Entling"

# Save "total_field_level_data" file

write.csv(FINAL_field_level_data_filt, "../Final_Data/CropPol_field_level_data.csv",row.names = F)


##############################
# MERGE INSECT_SAMPLING DATA
##############################

# List of files (in our data repository folder) whose name begins with
# "insect_sampling"

list_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]


extract_sampling_i <- function(file_name){
  sampling_i <- read_csv(file_name,
                            col_types = cols(
                              study_id = col_character(),
                              site_id = col_character(),
                              pollinator = col_character(),
                              guild = col_character(),
                              sampling_method = col_character(),
                              abundance = col_double(),
                              total_sampled_area = col_double(),
                              total_sampled_time = col_double(),
                              total_sampled_flowers = col_double(),
                              Description = col_character()
                              ))
  sampling_i
}

#Test data files and save the results

test_file("../testthat/test-format-insect_sampling.R", reporter = "summary") #Visualize testing
options(testthat.output_file = "../testthat/test_out_Create_Dataset_I.txt")
# test_file("../testthat/test-format.R", reporter = "summary")

# Extract the names of the files which contain failures

report <- readLines("../testthat/test_out_Create_Dataset_I.txt")

file.failures <- str_match(report, "insect_sampling_(.*?)csv")
file.failures <- file.failures[!is.na(file.failures[,1]),1]
file.failures <- file.failures[!duplicated(file.failures)]

# Merge the files without failures

list_files_insect_sampling <- list_files_insect_sampling[!list_files_insect_sampling %in% file.failures]


for (i in seq(length(list_files_insect_sampling))) {
  
  sampling_i <- paste(folder_base, list_files_insect_sampling[i], sep = "/")
  sampling_i <- extract_sampling_i(sampling_i)
  
  if (i == 1) {
    FINAL_sampling_data <- sampling_i
  }
  else {
    FINAL_sampling_data <- FINAL_sampling_data %>% bind_rows(sampling_i)
  }
  
}

FINAL_sampling_data <- FINAL_sampling_data %>% filter(abundance > 0)

FINAL_sampling_data %>% group_by(guild) %>% count()

# fix guilds 

FINAL_sampling_data$guild[FINAL_sampling_data$guild=="bombyliidae"] <- "humbleflies"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="other wild bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="wild_bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="others"] <- "other"

FINAL_sampling_data %>% group_by(guild) %>% count()


# Select verified studies

FINAL_sampling_data_filt <- FINAL_sampling_data %>% filter(study_id %in%
                                                                   verified_studies$study_id)

# Sanity check: All the studies are in field level data
FINAL_sampling_data_filt$study_id[!FINAL_sampling_data_filt$study_id %in%
                                    FINAL_field_level_data$study_id]


##############################
# ADD TAXA RESOLUTION
##############################

# Load taxon (DAINESE)
dainese_taxon <- read_csv("Taxon_info/taxon_DAINESE_corrected.csv") %>% unique() %>%
  rename(pollinator=Organism_ID)

# Load taxon Rader
rader_taxon <- read_csv("Taxon_info/taxon_table_Rader.csv") %>%
  dplyr::select(-matched_name)

# Load taxon Silvia
silvia_taxon <- read_csv("Taxon_info/taxon_Silvia_simple.csv")

# Load other taxon 

other_taxon <- read_csv("Taxon_info/taxon_other_studies.csv")


# Load final insect sampling
insect_sampling <- FINAL_sampling_data_filt

insect_sampling %>% filter(study_id %in% 
                             c(dainese_taxon$study_id)) #12,042 entries


add_dainese <- insect_sampling %>%
  left_join(dainese_taxon,by=c("study_id","sampling_method","pollinator")) 

# Fix You lepidoptera families

add_dainese$rank[add_dainese$study_id=="Yi_Zou_Brassica_napus_China_2015" &
                   is.na(add_dainese$rank)] <- "family"


add_dainese %>% filter(is.na(rank)) #28,057 entries need resolution


add_dainese_rader <- add_dainese %>%
  filter(!study_id %in% 
           c("Silvia_Castro_Actinidia_deliciosa_Portugal_2018",
             "Silvia_Castro_Actinidia_deliciosa_Portugal_2019",
             "Silvia_Castro_Helianthus_annuus_Spain_2017",
             "Silvia_Castro_Helianthus_annuus_Spain_2018")) %>%
  left_join(rader_taxon, by= "pollinator") %>%
  mutate(rank = coalesce(rank.x, rank.y)) %>%
  dplyr::select(-rank.x, -rank.y)%>%
  left_join(other_taxon, by= "pollinator") %>%
  mutate(rank = coalesce(rank.x, rank.y),
         notes = coalesce(notes.x, notes.y)) %>%
  dplyr::select(-rank.x, -rank.y,-notes.x, -notes.y)

x <- add_dainese_rader %>% filter(is.na(rank)) %>% select(pollinator) %>% unique()


add_Silvia <- insect_sampling %>%
  filter(study_id %in% 
           c("Silvia_Castro_Actinidia_deliciosa_Portugal_2018",
             "Silvia_Castro_Actinidia_deliciosa_Portugal_2019",
             "Silvia_Castro_Helianthus_annuus_Spain_2017",
             "Silvia_Castro_Helianthus_annuus_Spain_2018")) %>%
  left_join(silvia_taxon,by=c("study_id","pollinator"))

# Corrections Silvia

silvia_correction <- insect_sampling %>%
  filter(study_id %in% 
           c("Silvia_Castro_Actinidia_deliciosa_Portugal_2018",
             "Silvia_Castro_Actinidia_deliciosa_Portugal_2019",
             "Silvia_Castro_Helianthus_annuus_Spain_2017",
             "Silvia_Castro_Helianthus_annuus_Spain_2018")) %>%
  filter(sampling_method!="census") %>%
  group_by(study_id,site_id,pollinator,guild,sampling_method,
           total_sampled_area,total_sampled_time,total_sampled_flowers,
           Description) %>% count() %>% filter(n>1)

for (i in 1:nrow(silvia_correction)){
  add_Silvia$rank[add_Silvia$study_id==silvia_correction$study_id[i]&
                    add_Silvia$site_id==silvia_correction$site_id[i]&
                    add_Silvia$pollinator==silvia_correction$pollinator[i]&
                    add_Silvia$guild==silvia_correction$guild[i]&
                    add_Silvia$sampling_method==silvia_correction$sampling_method[i]
                  ] <- "morphospecies"
  
  add_Silvia$notes[add_Silvia$study_id==silvia_correction$study_id[i]&
                     add_Silvia$site_id==silvia_correction$site_id[i]&
                     add_Silvia$pollinator==silvia_correction$pollinator[i]&
                     add_Silvia$guild==silvia_correction$guild[i]&
                     add_Silvia$sampling_method==silvia_correction$sampling_method[i]
                   ] <- "According to the corresponding author, if there are several pan-trap records for a given species at a given site, it means that such record was identified to a morphospecies."
}

insect_sampling_taxa <- bind_rows(add_dainese_rader,add_Silvia) %>%
  select(study_id, site_id, sampling_method,pollinator, rank, guild, 
         sampling_method,abundance,total_sampled_area,total_sampled_time,
         total_sampled_flowers,Description,notes) %>%
  rename(identified_to=rank,description=Description) %>% unique()


# Save "FINAL_sampling_data" file

write.csv(insect_sampling_taxa, "../Final_Data/CropPol_sampling_data.csv",row.names = F
)


