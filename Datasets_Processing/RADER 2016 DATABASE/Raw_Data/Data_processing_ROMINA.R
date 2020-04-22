
library(tidyverse)
library("iNEXT")
library(openxlsx)


dir_ini <- getwd()

##########################
#Data: Rader et al. 2016 
##########################

data_raw <- read.xlsx("master_nonbeemay24sep14.xlsx",
                          sheet = "master_nonbeemay2814", startRow = 1)

##########################################
# FIXING COLUMNS' NAMES
# Two columns names are repeated and they contain different values, respectively
#"Sphaerophoria_sp." "Syritta_pipiens"
# Solution proposed: To merged columns with the same name

# Fix format of observations
data_raw[,30:ncol(data_raw)] <- sapply(data_raw[,30:ncol(data_raw)], as.numeric)


names_data_raw <- names(data_raw)
names_data_raw[duplicated(names_data_raw)]
#"Sphaerophoria_sp." "Syritta_pipiens"
names(data_raw)[408]==names(data_raw)[412]


which(colnames(data_raw)=="Sphaerophoria_sp.")
sum(!is.na(data_raw[,408]))
sum(!is.na(data_raw[,412]))

# Combine both columns and delete one

data_raw[,408] <- rowSums(data_raw[,c(408, 412)], na.rm=TRUE)
data_raw[,412] <- NULL #remove column

# Sanity check: new column 412 is different from column 408
names(data_raw)[408]==names(data_raw)[412]

which(colnames(data_raw)=="Syritta_pipiens")
sum(!is.na(data_raw[,418]))==sum(!is.na(data_raw[,432]))

data_raw[,418] <- rowSums(data_raw[,c(418, 432)], na.rm=TRUE)
data_raw[,432] <- NULL
# Sanity check: new column 418 is different from column 432
names(data_raw)[418]==names(data_raw)[432]

###############################################
#Organism list

organism_guild <- tibble(Organism_ID = names(data_raw)[30:ncol(data_raw)])

gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")

organism_guild <- organism_guild %>% left_join(gild_list,by=c("Organism_ID"))

#Check NA's in guild
organism_guild %>% filter(is.na(Guild)) %>% group_by(Organism_ID,Family) %>% count() #No NA's

###############################################

data_raw <- as_tibble(data_raw)

#authors <- data_raw %>% group_by(author,crop,Year_of_study) %>% count()
#write.csv(authors,"authors_list.csv")

#Remove studies that were included in Dainese et al. 2019

authors_in_Dainese <- c("Anderson","Bartomeus","Carvalheiro",
                        "Chacoff","Freitas","Garratt_potts",
                        "garrett","Howlett","Stanley_stout",
                        "taki")

data_filter <- data_raw %>% filter(!author %in% authors_in_Dainese)

# Filter the works by Smitha
data_filter <- data_filter %>% filter(!(author=="smitha" & Year_of_study> 1900))


resultados <- data_filter %>% group_by(author,crop,Year_of_study) %>%
  summarise(number_points = length(latitude),lat_mean = sum(is.na(latitude)),
            mean_Inflorescences_half_m2 = sum(is.na(Inflorescences_half_m2)),
            mean_Flowers_per_Inflorescence = sum(is.na(Flowers_per_Inflorescence)),
            mean_flowers_observed = sum(is.na(flowers_observed)),
            mean_fruitset = sum(is.na(fruitset)),
            mena_final_fruitset= sum(is.na(final_fruitset)))

# Save studies in individual CSV files
for (i in 1:nrow(resultados)){

  dataset_i <- data_filter %>% filter(author==resultados$author[i],
                                      crop==resultados$crop[i],
                                      Year_of_study==resultados$Year_of_study[i])
  path_i = paste0("Individual CSV/",resultados$author[i],"_",resultados$Year_of_study[i],".csv")
  print(path_i)
  write_csv(dataset_i,path_i)
}

#######################################33
# EXAMPLE BARTOMEUS
bartomeus_data <- data_raw %>% filter(author=="Bartomeus")


bartomeus_data_obs <- bartomeus_data %>%
  select(site,round,row,observation_location, names(bartomeus_data[30:ncol(bartomeus_data)]))


bartomeus_data_g <- bartomeus_data_obs %>% 
  filter(!observation_location %in% c("Control_Edge","Control_Crop")) %>%
  group_by(site,round,row,observation_location) %>% summarise_all(funs(sum))

# Remove columns full of NA's
bartomeus_data_g <- 
  bartomeus_data_g[,colSums(is.na(bartomeus_data_g))<nrow(bartomeus_data_g)]

x <- bartomeus_data_g %>% select(-round,-observation_location) %>% 
  group_by(site) %>% summarise_all(funs(sum))

dir_ini <- getwd()
setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_Processing/POLLINATION DATABASE - DAINESE-20200218T092444Z-001/DATASETS/")
bart_raw <- read.xlsx("Bart01_DataCollection_Pollination.xlsx",
                      sheet = "SpeciesData", startRow = 2)
setwd(dir_ini)
bart_raw %>% group_by(SiteID,OrganismID) %>% summarise(Abundance=sum(Abundance)) %>% head(15)
