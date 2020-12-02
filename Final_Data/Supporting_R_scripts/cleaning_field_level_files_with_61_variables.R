
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

# extract_template_i: function that reads field_level_data files

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

######################################
# 0 List of field_level_data files
######################################

# List of files (in our data repository folder: Processing_files/Datasets_storage)
folder_base <- "Processing_files/Datasets_storage"
files_base <- list.files(folder_base)

# List of files (in our data repository folder) whose name begins with
# "field_level_data" and have less than 65 columns

list_all_files_field_level <- files_base[grepl("field_level_data", files_base)]
list_files_field_level <- NULL
list_files_field_level_65 <- NULL

for(i in 1:length(list_all_files_field_level)){

  file_i <- paste(folder_base, list_all_files_field_level[i], sep = "/")
  csv_i <- read_csv(file_i)

  if(ncol(csv_i) < 65){
    list_files_field_level <- c(list_files_field_level,list_all_files_field_level[i])
  }else{
    list_files_field_level_65 <- c(list_files_field_level_65,list_all_files_field_level[i])
  }

}

##########################################
# 1 Test data files and save the results
##########################################

# Run test and extract report on files which contain failures
report <- capture_output_lines({
  test_file("testthat/test-format-field_level.R", reporter = "summary")
})

file.failures <- str_match(report, "file_(.*?).csv")
file.failures <- list_all_files_field_level[
  as.numeric(file.failures[!is.na(file.failures[,1]),2])]
file.failures <- file.failures[!duplicated(file.failures)]

# Run country test and extract report on files which contain failures
# report <- capture_output_lines({
#   test_file("testthat/test-country.R", reporter = "summary")
# })
#
# file.failures <- str_match(report, "file_(.*?).csv")
# file.failures <- list_files_field_level[
#   as.numeric(file.failures[!is.na(file.failures[,1]),2])]
# file.failures <- file.failures[!duplicated(file.failures)]

###########################################
# 2 MERGE FIELD_LEVEL DATA WITHOUT ERRORS
###########################################

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

##########################
# 3 DATA CLEANING
##########################

# Fix crop names-----
FINAL_field_level_data$crop %>% unique() %>% sort()

FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Malus Domestica"] <- "Malus domestica"
FINAL_field_level_data$crop[FINAL_field_level_data$crop=="Fragaria × ananassa"] <- "Fragaria x ananassa"

# Fix country-----
FINAL_field_level_data$country[FINAL_field_level_data$country=="United States"] <- "USA"
FINAL_field_level_data$country %>% unique() %>% sort()

# Fix variety-----
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Koipesol Oleko"] <- "Koipesol OLEKO"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="dk exclaim, dk exsenntial"] <- "DK Exclaim, DK Exsenntial"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="elgar"] <- "Elgar"

FINAL_field_level_data$variety[FINAL_field_level_data$variety=="keitt"] <- "Keitt"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="KEITT"] <- "Keitt"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="KENT"] <- "Kent"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="kerbel"] <- "Kerbel"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="mixed"] <- "Mixed"

FINAL_field_level_data$variety[FINAL_field_level_data$variety=="NP, Butte, Padre?"] <- "NP, Butte, Padre"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Over 60 varieties"] <- "Mixed (over 60 varieties)"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="phoenix"] <- "Phoenix"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="pinks mammoth"] <- "Pinks mammoth"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Unknown -Brreeding Plot"] <- "Unknown -Breeding Plot"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="varied"] <- "Mixed"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="wild"] <- "Wild"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="wild form"] <- "Wild"

FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Conference & Comice"] <- "Conference, Comice"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Conference & Gieser Wildeman"] <- "Conference, Gieser Wildeman"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="DY3 + Shelly"] <- "DY3, Shelly"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Elstar& Jonagold"] <- "Elstar, Jonagold"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="K & L*"] <- "K, L*"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Medium term variety_6 months"] <- "Medium term variety (6 months)"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="Monterey + Nonpareil + Peerless"] <- "Monterey, Nonpareil, Peerless"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="old trees: Price, Peerless, NP, younger trees:IXL, Drake, Mission, NP"] <- "Old trees: Price, Peerless, NP, younger trees:IXL, Drake, Mission, NP"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="picto"] <- "Picto"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="unknown"] <- "Unknown"
FINAL_field_level_data$variety[FINAL_field_level_data$variety=="B & C*"] <- "B, C*"


FINAL_field_level_data$variety <- gsub("é","e",FINAL_field_level_data$variety)
FINAL_field_level_data$variety <- gsub("è","e",FINAL_field_level_data$variety)
FINAL_field_level_data$variety <- gsub("ó","o",FINAL_field_level_data$variety)

FINAL_field_level_data$variety %>% unique() %>% sort()

# Fix pollinator restriction-----

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
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="Bees, including honeybees; butterflies and diurnal moths"] <- "bees (including honeybees) and lepidoptera"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="Bees, syrphids and wasps"] <- "bees, syrphids and wasps"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="Honeybees are not included in richness metrics"] <- "honeybees are not included in richness metrics"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="only honey bees and bombus sp."] <- "only bees (honey bees and bumblebees)"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="only non-Apis bees"] <- "only bees (non-Apis bees)"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="only non-managed bees"] <- "only bees (non-managed bees)"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="only bumblebees"] <- "only bees (bumblebees)"
FINAL_field_level_data$richness_restriction[
  FINAL_field_level_data$richness_restriction=="bombus and other wild bees"] <- "only bees (bumblebees and other wild bees)"




FINAL_field_level_data$richness_restriction %>% unique() %>% sort()

#Fix other_richness_estimator_method-----

FINAL_field_level_data$other_richness_estimator_method[FINAL_field_level_data$other_richness_estimator_method=="Chao1"] <- "Chao 1"


# Fix Publication-----
FINAL_field_level_data$Publication %>% unique()

FINAL_field_level_data$Publication[FINAL_field_level_data$Publication==":10.1016/j.agee.2008.08.001"] <- "10.1016/j.agee.2008.08.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="https://doi.org/10.1038/s41598-019-49535-w; yield data unpublished"] <- "10.1038/s41598-019-49535-w; yield data unpublished"
FINAL_field_level_data$Publication[FINAL_field_level_data$study_id=="Alejandro_Trillo_Fragaria_ananassa_Spain_2016"] <- "10.1016/j.baae.2018.05.008"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="none - writing in progress"] <- "In preparation"

FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="Carvalheiro et al. 2011. Ecology Letters"] <- "10.1111/j.1461-0248.2010.01579.x"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="Carvalheiro et al. 2010. Journal of applied ecology"] <-  "10.1111/j.1365-2664.2010.01829.x"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="Carvalheiro et al 2012 Journal of Applied Ecology"] <- "10.1111/j.1365-2664.2012.02217.x"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="https://doi.org/10.1038/s41598-019-49535-w; yield data unpublished"] <- "10.1038/s41598-019-49535-w; yield data unpublished"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="J. Pollinat. Ecol., 12 (2014), pp. 15-21"] <- "10.1590/1519-6984.02213"

FINAL_field_level_data$Publication[FINAL_field_level_data$Publication==":10.1016/j.agee.2008.08.001"] <- "10.1016/j.agee.2008.08.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="?10.3390/d12060259"] <- "10.3390/d12060259"

FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.001"] <-  "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.002"] <-   "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.003"] <- "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.004"] <- "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.005"] <- "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.006"] <-  "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.007"] <-  "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.008"] <-  "unpublished, 10.1016/j.biocon.2013.11.001"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, O'Connor et al. 2019 https://doi.org/10.1111/2041-210X.13292"] <-  "unpublished, 10.1111/2041-210X.13292"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2016 https://doi.org/10.1371/journal.pone.0153889, Garratt et al. 2014 DOI:10.26786/1920-7603(2014)8, O'Connor et al. 2019 https://doi.org/10.1111/2041-210X.13292"] <- "unpublished, 10.1371/journal.pone.0153889, 10.26786/1920-7603(2014)8,10.1111/2041-210X.13292"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="unpublished, Garratt et al. 2014 https://doi.org/10.1016/j.biocon.2013.11.001, O'Connor et al 2019 https://doi.org/10.1111/2041-210X.13292"] <-"unpublished, 10.1016/j.biocon.2013.11.001, 10.1111/2041-210X.13292"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1098/rspb.2013.3148, http://dx.doi.org/10.5281/zenodo.12540"] <-  "10.1098/rspb.2013.3148, 10.5281/zenodo.12540"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="j.agee.2016.04.020"] <-  "10.1016/j.agee.2016.04.020"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1111/j.1744-7348.2009.00326.x/10.1016/j.baae.2010.08.004"] <-  "10.1111/j.1744-7348.2009.00326.x, 10.1016/j.baae.2010.08.004"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1126/science.aac7287; 10.26786/1920-7603%282014%2926"] <-  "10.1126/science.aac7287, 10.26786/1920-7603%282014%2926"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1016/j.agee.2018.10.018; 10.1016/j.agee.2017.08.030"] <-  "10.1016/j.agee.2018.10.018, 10.1016/j.agee.2017.08.030"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1038/s41598-019-49535-w; yield data unpublished"] <-  "10.1038/s41598-019-49535-w, yield data unpublished"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1016/j.agee.2018.05.004; 10.1016/j.agee.2019.02.009"] <-  "10.1016/j.agee.2018.05.004, 10.1016/j.agee.2019.02.009"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1111/j.1365-2664.2005.01116.x,10.1098/rspb.2007.1547"] <-  "10.1111/j.1365-2664.2005.01116.x, 10.1098/rspb.2007.1547"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1073/pnas.0405147101,10.1111/j.1523-1739.2004.00227.x"] <-  "10.1073/pnas.0405147101, 10.1111/j.1523-1739.2004.00227.x"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="10.1007/s13593-016-0377-7,10.1016/j.agee.2012.05.003,10.1073/pnas.1210590110"] <-  "10.1007/s13593-016-0377-7, 10.1016/j.agee.2012.05.003, 10.1073/pnas.1210590110"
FINAL_field_level_data$Publication[FINAL_field_level_data$Publication=="npublished, 10.1371/journal.pone.0153889, 10.26786/1920-7603(2014)8,10.1111/2041-210X.13292"] <-  "npublished, 10.1371/journal.pone.0153889, 10.26786/1920-7603(2014)8, 10.1111/2041-210X.13292"



FINAL_field_level_data$Publication %>% unique()


# Fix Sampling years-----

FINAL_field_level_data$sampling_year %>% unique()
FINAL_field_level_data$sampling_year[FINAL_field_level_data$sampling_year=="2000/2001"] <- "2000-2001"
FINAL_field_level_data$sampling_year[FINAL_field_level_data$sampling_year=="2016/2017"] <- "2016-2017"
FINAL_field_level_data$sampling_year[FINAL_field_level_data$sampling_year=="2015/2016"] <- "2015-2016"

FINAL_field_level_data$sampling_year %>% unique()


# Fix studies IDs-----
FINAL_field_level_data$study_id[grep("Christof_Sch",FINAL_field_level_data$study_id)] <-
  "Christof_Schüepps_Prunus_avium_Switzerland_2011"


FINAL_field_level_data$study_id[grep("several",FINAL_field_level_data$study_id)] %>%
  unique()

FINAL_field_level_data %>% filter(study_id=="Jens_Åström_Trifolium_pratense_Norway_several_years") %>%
  select(sampling_year) %>% unique()

FINAL_field_level_data$study_id[(FINAL_field_level_data$study_id==
                                   "Jens_Åström_Trifolium_pratense_Norway_several_years") &
                                  (FINAL_field_level_data$sampling_year==2013)] <-
  "Jens_Åström_Trifolium_pratense_Norway_2013"

FINAL_field_level_data$study_id[(FINAL_field_level_data$study_id==
                                   "Jens_Åström_Trifolium_pratense_Norway_several_years") &
                                  (FINAL_field_level_data$sampling_year==2014)] <-
  "Jens_Åström_Trifolium_pratense_Norway_2014"



# Fix total_sampled_area and time-----

FINAL_field_level_data$total_sampled_area %>% unique()


FINAL_field_level_data$notes <- NA

FINAL_field_level_data$notes[grep("Alejandro_Trillo_",
                                  FINAL_field_level_data$study_id)] <- "total_sampled_area: 800 m2 for honeybees and bumblebees, otherwise 400 m2"
FINAL_field_level_data$notes[grep("Amparo",
                                  FINAL_field_level_data$study_id)] <- "total_sampled_area: 20 almond individuals; 5-10  meters separation between individuals"
FINAL_field_level_data$total_sampled_area[grep("Amparo",
                                               FINAL_field_level_data$study_id)] <- NA

FINAL_field_level_data$total_sampled_area[grep("Alejandro_Trillo_",
                                               FINAL_field_level_data$study_id)] <- 800


FINAL_field_level_data$total_sampled_area <- as.numeric(FINAL_field_level_data$total_sampled_area)


# Add notes on richness-----

FINAL_field_level_data$notes[grep("Rachel_Mallinger",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from bee bowl records. Richness can be calculated by using the sampling records for this study_id."

FINAL_field_level_data$notes[grep("Heather",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from bee bowl records. Richness can be calculated by using the sampling records for this study_id."

FINAL_field_level_data$notes[grep("Heather",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from bee bowl records. Richness can be calculated by using the sampling records for this study_id."

FINAL_field_level_data$notes[grep("Yi_Zou",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from bee bowl records. Richness can be calculated by using the sampling records for this study_id."

FINAL_field_level_data$notes[grep("Georg_Andersson_Fragaria_ananassa",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from pan-trap records. Richness can be calculated by using the sampling records for this study_id."

FINAL_field_level_data$notes[grep("Breno_M_Freitas_Gossypium_hirsutum_Brazil_2011",
                                  FINAL_field_level_data$study_id)] <-
  "Information on floral visitors was obtained from pan-trap records. Richness can be calculated by using the sampling records for this study_id."



# Fix yield units-----

x <- FINAL_field_level_data %>% select(yield, yield_units) %>% group_by(yield_units) %>%
  summarise(yield=mean(yield,na.rm=T))



x$yield_units

FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="% fruit set"] <- "fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="% fruit set per flowers"] <- "fruit set (%) per flowers"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="% fruit set per plant"] <-    "fruit set (%) per plant"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="% pod set"] <- "pod set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="early fruit set" ] <-    "early fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="final fruitset % (100*#fruits/#flowers)"] <- "final fruit set (%): 100*#fruits/#flowers"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="final fruitset (%)"] <- "final fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="final seedset: percentage of ovules that developed into seeds at harvest"] <- "final seed set (%): ovules that developed into seeds at harvest"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit-set (% of flowers setting fruits at harvest.)"] <- "fruit set (%): flowers setting fruits at harvest.)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit set %"] <-    "fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="fruit set (%)"] <-"fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit set (%)"] <- "fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit Set (%)"] <- "fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit set (%): initial number of floral buds in the respective branch and the number of developing fruits"] <-  "fruit set (%): initial number of floral buds in the respective branch and the number of developing fruits"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit set (%): percentage of intact marked flowers (not damaged by herbivores) that developed into swollen green fruits approximately three weeks after pollination per site"] <-"fruit set (%): percentage of intact marked flowers (not damaged by herbivores) that developed into swollen green fruits approximately three weeks after pollination per site"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Fruit weight per plant"] <-  "fruit weight per plant"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="fruitset (%)"] <-  "fruit set (%)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="kg/field"] <-  "kg per field"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="kg/ha"] <-  "kg per hectare"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="kg/ha (dried and hulled beans)"] <- "kg per ha (dried and hulled beans)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="kg/hectare"] <-    "kg per hectare"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="kg/shrub"] <-  "kg per shrub"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="t/ha"] <-     "tonne per hectare"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="tonne/acre" ] <- "tonne per acre"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="tonnes/ha"] <-  "tonne per hectare"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="bags/hectare"] <- "bags per hectare"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="g" ] <- "grams"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="g per fruit"] <- "grams per fruit"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="seed/plant"] <-  "seeds per plant"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Primary fruit weight [g]"] <- "Primary fruit weight (grams)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="mean_berry_weight"] <- "mean berry weight"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="grams (mean fruit weight for 10 fruits per field)"] <- "mean fruit weight for 10 fruits per field (grams)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="grams (average fruit weight)"] <- "average fruit weight (grams)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="final fruit set (%): 100*#fruits/#flowers"] <- "fruit set (%): 100 number of fruits/number of open flowers"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="percentage fruit set (100 number of fruits/number of open flowers)"] <- "fruit set (%): 100 number of fruits/number of open flowers"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="mean_berry_weight"] <- "mean berry weight"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Primary fruit weight (grams)"] <- "primary fruit weight (grams)"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="Insect Pollination = Open pollination [control] - Self-pollination [Tulle bags]"] <- "Insect Pollination: Open pollination [control] - Self-pollination [Tulle bags]"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="dry weight; mean kgs per tree"] <- "dry weight: mean kgs per tree"
FINAL_field_level_data$yield_units[FINAL_field_level_data$yield_units=="fruit set (weight in g of 100 randomly selected seeds)"] <- "fruit set: weight in g of 100 randomly selected seeds"


FINAL_field_level_data$yield_units %>% unique() %>% sort()

# Fix yield units-----

x <- FINAL_field_level_data %>% select(yield2, yield2_units) %>% group_by(yield2_units) %>%
  summarise(yield2=mean(yield2,na.rm=T))

FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Seed per pod"] <- "seeds per pod"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Seed set"] <- "seed set"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="number of fruits per_flower"] <- "number of fruits per flower"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="mean_marketability"] <- "mean marketability"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Mean weight per apple (g per apple)"] <- "mean weight per apple (grams per apple)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Mean weight per bean pod (g)"] <- "mean weight per bean pod (grams)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Individual fruit weight (gr)"] <- "individual fruit weight (grams)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Fruit set (% of flowers producing a berry)"] <- "fruit set (%): flowers producing a berry)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="kg/ha (dried berries)"] <- "kg per hectare (dried berries)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="kg/m2"] <- "kg per square meter (dried berries)"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="Fruit number on fixed branch length per tree"] <- "number of fruits on fixed branch length per tree"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="final fruitset: percentage of flowers that developed into mature fruits at harvest"] <- "final fruitset (%): flowers that developed into mature fruits at harvest"
FINAL_field_level_data$yield2_units[FINAL_field_level_data$yield2_units=="seedset (number of seeds per pod)"] <- "seeds per pod"


FINAL_field_level_data$yield2_units %>% unique()


# Fix management-----

FINAL_field_level_data %>% group_by(management) %>% count()

FINAL_field_level_data$management[FINAL_field_level_data$management %in%
                                    c("3","conv","Conventional",
                                      "conventional agriculture")] <- "conventional"
FINAL_field_level_data$management[FINAL_field_level_data$management %in%
                                    c("org","Organic")] <- "organic"


# Fix visitation rate units-----

FINAL_field_level_data$visitation_rate_units %>% unique()

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "flower visits per plant and hour"] <- "visits per plant and hour"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "flowers_visited/min"] <- "flowers_visited per min"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "flower visits/m^2/10 mins"] <- "visits per m^2 and 10 mins"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits in 100 flowers during one hour"] <- "visits per 100 flowers and one hour"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per 100 flowers during 1 hour"] <- "visits per 100 flowers and 1 hour"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "Visits per 10 minute"] <- "Visits per 10 minutes"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per hour and plant"] <- "visits per plant and hour"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits to flowers per hour"] <- "visits per hour"
FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "no. visits in 100 inflorescences per 1h"] <- "visits per 100 inflorescences and hour"



FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per hour and bush"] <- "visits per bush and hour"

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per 100 flowers and one hour"] <- "visits per 100 flowers and hour"

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per 100 flowers and 1 hour"] <- "visits per 100 flowers and hour"

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "flowers_visited per min"] <- "flowers visited per min"

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visits per m^2 and hour"] <- "visits per hour and m2"

FINAL_field_level_data$visitation_rate_units[
  FINAL_field_level_data$visitation_rate_units==
    "visited flowers per hour"] <- "flowers visited per hour"


FINAL_field_level_data$visitation_rate_units %>% unique() %>% sort()


###########################
# 4 Select verified studies
###########################

# List of verified datasets with 61 variables

verified <- read_excel("Final_Data/Supporting_files/Verified_studies/FINAL_Data ownership.xlsx")
verified_studies <- verified %>% select(study_id) %>% unique()

# Select only verified studies

FINAL_field_level_data_filt <- FINAL_field_level_data %>% filter(study_id %in%
                                                                   c(verified_studies$study_id,
                                                                     "Jens_Åström_Trifolium_pratense_Norway_2013",
                                                                     "Jens_Åström_Trifolium_pratense_Norway_2014"))

FINAL_field_level_data_filt$Credit[grep("Christof Sch",FINAL_field_level_data_filt$Credit)] <- "Christof Schüepps, Felix Herzog and Martin H. Entling"


###########################
# 5 Add the 3 new variables
###########################

sampling_methods <- read_csv("Final_Data/Supporting_files/Sampling_methods/table_sampling_methods_Filled.csv") %>%
  select(study_id,sampling_richness,sampling_abundance,sampling_visitation)

FINAL_field_level_data_filt <- FINAL_field_level_data_filt %>%
  left_join(sampling_methods,by="study_id")

FINAL_field_level_data_filt$sampling_richness %>% unique() %>% sort()

FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Beebowl"] <- "pan trap, bee bowl, blue vane trap, pitfall"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Focal"] <- "focal observations"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Net"] <- "sweep net"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Net+Beebowl"] <- "sweep net + pan trap, bee bowl, blue vane trap, pitfall"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Net+Focal"] <- "sweep net + focal observations"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="Net+other"] <- "sweep net + other"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="transect"] <- "transects"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="transect+focal"] <- "transects + focal observations"
FINAL_field_level_data_filt$sampling_richness[
  FINAL_field_level_data_filt$sampling_richness=="transect+pantrap"] <- "transects + pan trap, bee bowl, blue vane trap, pitfall"

FINAL_field_level_data_filt$sampling_richness %>% unique() %>% sort()

FINAL_field_level_data_filt$sampling_abundance %>% unique() %>% sort()

FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Beebowl"] <- "pan trap, bee bowl, blue vane trap, pitfall"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Focal"] <- "focal observations"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Net"] <- "sweep net"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Net+Beebowl"] <- "sweep net + pan trap, bee bowl, blue vane trap, pitfall"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Net+Focal"] <- "sweep net + focal observations"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="Net+other"] <- "sweep net + other"
FINAL_field_level_data_filt$sampling_abundance[
  FINAL_field_level_data_filt$sampling_abundance=="transect"] <- "transects"

FINAL_field_level_data_filt$sampling_abundance %>% unique() %>% sort()

FINAL_field_level_data_filt$sampling_visitation %>% unique() %>% sort()

FINAL_field_level_data_filt$sampling_visitation[
  FINAL_field_level_data_filt$sampling_visitation=="Focal"] <- "focal observations"
FINAL_field_level_data_filt$sampling_visitation[
  FINAL_field_level_data_filt$sampling_visitation=="Net"] <- "sweep net"
FINAL_field_level_data_filt$sampling_visitation[
  FINAL_field_level_data_filt$sampling_visitation=="transect"] <- "transects"

FINAL_field_level_data_filt$sampling_visitation %>% unique() %>% sort()


# Reorder columns by (column) name

FINAL_field_level_data_filt <- FINAL_field_level_data_filt[c(
  "study_id","site_id","crop","variety" ,"management","country",
  "latitude","longitude","X_UTM","Y_UTM","zone_UTM","sampling_start_month",
  "sampling_end_month","sampling_year","field_size","yield","yield_units","yield2",
  "yield2_units","yield_treatments_no_pollinators",
  "yield_treatments_pollen_supplement","yield_treatments_no_pollinators2",
  "yield_treatments_pollen_supplement2","fruits_per_plant","fruit_weight","plant_density",
  "seeds_per_fruit","seeds_per_plant",
  "seed_weight","sampling_richness","observed_pollinator_richness",
  "other_pollinator_richness","other_richness_estimator_method",
  "richness_restriction","sampling_abundance","abundance","ab_honeybee",
  "ab_bombus","ab_wildbees","ab_syrphids","ab_humbleflies","ab_other_flies",
  "ab_beetles","ab_lepidoptera","ab_nonbee_hymenoptera","ab_others",
  "total_sampled_area","total_sampled_time","sampling_visitation",
  "visitation_rate_units","visitation_rate","visit_honeybee","visit_bombus",
  "visit_wildbees","visit_syrphids","visit_humbleflies","visit_other_flies",
  "visit_beetles","visit_lepidoptera","visit_nonbee_hymenoptera",
  "visit_others","Publication","Credit","Email_contact","notes")]

###############################
# 6 Remove non-ascii characters
###############################

df <- FINAL_field_level_data_filt %>% select(study_id,site_id,Credit)
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×", "aeiounuAoacaAENOax",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×", "aeiounuAoacaAENOax",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×", "aeiounuAoacaAENOax",df[r,3])
}

df$Credit[grep("Hajnalka",df$Credit)] <- "Hajnalka Szentgyorgyi, Michal Woyciechowski"

tools::showNonASCII(df$study_id) %>% unique()
tools::showNonASCII(df$site_id) %>% unique()
tools::showNonASCII(df$Credit) %>% unique()

FINAL_field_level_data_filt$study_id <- df$study_id
FINAL_field_level_data_filt$site_id <- df$site_id
FINAL_field_level_data_filt$Credit <- df$Credit
