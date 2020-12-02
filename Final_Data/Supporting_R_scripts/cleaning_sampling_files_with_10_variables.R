
# Processing field_level_data files with only 61 variables:
# 1) Specific test for field level files with 61 variables. Only those files without error
# will be used in the next steps.
# 2) Extract field level data without errors
# 3) Clean up the data without errors
# 4) Select only verified files
# 5) Add new variables
# 6) Remove non-ASCII characters

############
# FUNCTIONS

# extract_sampling_i: function that reads insect_sampling files

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

##################################
# 0 List of insect sampling files
##################################

# List of files (in our data repository folder: Processing_files/Datasets_storage)
folder_base <- "Processing_files/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "insect_sampling"

list_all_files_insect_sampling <- files_base[grepl("insect_sampling", files_base)]
list_files_insect_sampling <- NULL
list_files_insect_sampling_12 <- NULL

for(i in 1:length(list_all_files_insect_sampling)){

  file_i <- paste(folder_base, list_all_files_insect_sampling[i], sep = "/")
  csv_i <- read_csv(file_i)

  if(ncol(csv_i) < 12){
    list_files_insect_sampling <- c(list_files_insect_sampling,
                                    list_all_files_insect_sampling[i])
  }else{
    list_files_insect_sampling_12 <- c(list_files_insect_sampling_12,
                                       llist_all_files_insect_sampling[i])
  }

}


############################################
# 1 Test for sampling files with 10 variables
############################################

report <- capture_output_lines({
  test_file("testthat/test-format-insect_sampling.R", reporter = "summary")
})

# Extract the names of the files which contain failures

file.failures <- str_match(report, "file_(.*?).csv")
file.failures <- list_all_files_insect_sampling[
  as.numeric(file.failures[!is.na(file.failures[,1]),2])]
file.failures <- file.failures[!duplicated(file.failures)]

##############################################
# 2 MERGE INSECT_SAMPLING DATA WITHOUT ERRORS
##############################################

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

##########################
# 3 DATA CLEANING
##########################

FINAL_sampling_data <- FINAL_sampling_data %>% filter(abundance > 0)

# Fix guilds----

FINAL_sampling_data %>% group_by(guild) %>% count()

FINAL_sampling_data$guild[FINAL_sampling_data$guild=="bombyliidae"] <- "humbleflies"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="other wild bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="wild_bees"] <- "other_wild_bees"
FINAL_sampling_data$guild[FINAL_sampling_data$guild=="others"] <- "other"

FINAL_sampling_data %>% group_by(guild) %>% count()


# Sanity check
FINAL_sampling_data$study_id[grep("several",FINAL_sampling_data$study_id)] %>%
  unique() #"Jeroen_Scheper_Brassica_napus_several_countries_2012"

###########################
# 4 Select verified studies
###########################

# List of verified datasets with 61 variables

verified <- read_excel("Final_Data/Supporting_files/Verified_studies/FINAL_Data ownership.xlsx")
verified_studies <- verified %>% select(study_id) %>% unique()

# Select verified studies----

FINAL_sampling_data_filt <- FINAL_sampling_data %>% filter(study_id %in%
                                                             c(verified_studies$study_id,
                                                               "Jens_Åström_Trifolium_pratense_Norway_2013",
                                                               "Jens_Åström_Trifolium_pratense_Norway_2014"))
# Sanity check: All the studies are in field level data
FINAL_sampling_data_filt$study_id %>% unique()
FINAL_field_level_data_filt$study_id %>% unique()



###############################
# 5 Remove non-ascii characters
###############################

df <- FINAL_sampling_data_filt %>% select(study_id,site_id,pollinator,Description,sampling_method)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,3])
  df[r,4]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,4])
  df[r,5]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,5])
}

df$pollinator <- gsub("“", "", df$pollinator)
df$pollinator <- gsub("”", "", df$pollinator)
df$pollinator <- gsub("\u00A0", " ", df$pollinator, fixed = TRUE)
df$pollinator <- gsub("\u3000"," ", df$pollinator)
df$pollinator <- gsub(" nº","", df$pollinator)
df$Description <- gsub("±","+-", df$Description)


# tools::showNonASCII(df$study_id) %>% unique()
# tools::showNonASCII(df$site_id) %>% unique()
# tools::showNonASCII(df$pollinator) %>% unique()
# tools::showNonASCII(df$Description) %>% unique()
# tools::showNonASCII(df$sampling_method) %>% unique()

FINAL_sampling_data_filt$study_id <- df$study_id
FINAL_sampling_data_filt$site_id <- df$site_id
FINAL_sampling_data_filt$Description <- df$Description
FINAL_sampling_data_filt$sampling_method <- df$sampling_method
FINAL_sampling_data_filt$pollinator <- df$pollinator

# Sanity check: All study_ids in insect sampling are in field level
FINAL_sampling_data_filt$study_id[!FINAL_sampling_data_filt$study_id %in%
                                    FINAL_field_level_data_filt$study_id]


##########################################
# 6 Add new variables: identified_to, notes
##########################################

# Load taxon (DAINESE)
dainese_taxon <- read_csv("Final_Data/Supporting_files/Taxon_info/taxon_DAINESE_corrected.csv") %>% unique() %>%
  rename(pollinator=Organism_ID)


df <- dainese_taxon %>% select(study_id,sampling_method,pollinator,rank)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,3])
  df[r,4]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,4])
}

df$pollinator <- gsub("“", "", df$pollinator)
df$pollinator <- gsub("”", "", df$pollinator)
df$pollinator <- gsub("\u00A0", " ", df$pollinator, fixed = TRUE)
df$pollinator <- gsub("\u3000"," ", df$pollinator)
df$pollinator <- gsub(" nº","", df$pollinator)
df$sampling_method <- gsub("\u00A0", " ", df$sampling_method, fixed = TRUE)
df$rank <- gsub("\u00A0", " ", df$rank, fixed = TRUE)

# tools::showNonASCII(df$study_id) %>% unique()
# tools::showNonASCII(df$pollinator) %>% unique()
# tools::showNonASCII(df$sampling_method) %>% unique()
# tools::showNonASCII(df$rank) %>% unique()

dainese_taxon$study_id <- df$study_id
dainese_taxon$pollinator <- df$pollinator
dainese_taxon$sampling_method <- df$sampling_method
dainese_taxon$rank <- df$rank

# Load taxon Rader
rader_taxon <- read_csv("Final_Data/Supporting_files/Taxon_info/taxon_table_Rader.csv") %>%
  dplyr::select(-matched_name)

# tools::showNonASCII(rader_taxon$rank) %>% unique()
# tools::showNonASCII(rader_taxon$pollinator) %>% unique()

# Load taxon Silvia
silvia_taxon <- read_csv("Final_Data/Supporting_files/Taxon_info/taxon_Silvia_simple.csv")

df <- silvia_taxon %>% select(study_id,pollinator,rank,notes)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,3])
  df[r,4]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ë²", "aeiounuAoacaAENOaxe2",df[r,4])
}

df$pollinator <- gsub("“", "", df$pollinator)
df$pollinator <- gsub("”", "", df$pollinator)
df$pollinator <- gsub("\u00A0", " ", df$pollinator, fixed = TRUE)
df$pollinator <- gsub("\u3000"," ", df$pollinator)
df$pollinator <- gsub(" nº","", df$pollinator)
df$rank <- gsub("\u00A0", " ", df$rank, fixed = TRUE)

# tools::showNonASCII(df$study_id) %>% unique()
# tools::showNonASCII(df$pollinator) %>% unique()
# tools::showNonASCII(df$rank) %>% unique()
# tools::showNonASCII(df$notes) %>% unique()

silvia_taxon$study_id <- df$study_id
silvia_taxon$pollinator <- df$pollinator
silvia_taxon$rank <- df$rank
silvia_taxon$notes <- df$notes

# Load other taxon

other_taxon <- read_csv("Final_Data/Supporting_files/Taxon_info/taxon_other_studies.csv")
other_taxon$pollinator <- gsub("ë", "e", other_taxon$pollinator)
other_taxon$notes <- gsub("“", "", other_taxon$notes)
other_taxon$notes <- gsub("”", "", other_taxon$notes)

# tools::showNonASCII(other_taxon$pollinator) %>% unique()
# tools::showNonASCII(other_taxon$rank) %>% unique()
# tools::showNonASCII(other_taxon$notes) %>% unique()

# Load final insect sampling
insect_sampling <- FINAL_sampling_data_filt

insect_sampling %>% filter(study_id %in%
                             c(dainese_taxon$study_id)) #15,253 entries


add_dainese <- insect_sampling %>%
  left_join(dainese_taxon,by=c("study_id","sampling_method","pollinator"))

# Fix You lepidoptera families

add_dainese$rank[add_dainese$study_id=="Yi_Zou_Brassica_napus_China_2015" &
                   is.na(add_dainese$rank)] <- "family"


add_dainese %>% filter(is.na(rank)) #31,619 entries need resolution


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

add_dainese_rader %>% filter(is.na(rank)) %>% select(study_id) %>% unique()

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
  rename(identified_to=rank,description=Description)
