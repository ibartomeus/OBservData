
# To create data_ownership csv for the initial 189 studies


library(tidyverse)# to handle data
library(readxl) #to read xlsx files

data_ownership <-
  read_xlsx("Final_Data/Supporting_files/Verified_studies/FINAL_Data ownership.xlsx") %>%
  as_tibble() %>%
  rename(name=Name,
         affiliation=Affiliation,
         role=Role,
         funding=`Acknowledgments/Funding`
         ) %>%
  select(study_id,name,affiliation, email,role,funding)

##########################################
# Clean diacritic (non-ascii) characters

df <- data_ownership
for (r in 1:nrow(df)) {
  df[r,1]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,1])
  df[r,2]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,2])
  df[r,3]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,3])
  df[r,4]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,4])
  df[r,5]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,5])
  df[r,6]<-chartr("áéíóúñüÅöàçãÁÉÑÖå×ôØêâäèëø", "aeiounuAoacaAENOaxoOeaaeeo",df[r,6])
}

df$name[grep("Woyciechowski",df$name)] <- "Michal Woyciechowski"
df$name <- gsub("\u00A0", " ", df$name, fixed = TRUE)
df$name <- gsub("\u3000"," ", df$name)

df$affiliation <- gsub("\u00A0", " ", df$affiliation, fixed = TRUE)
df$affiliation <- gsub("\u3000"," ", df$affiliation)

df$affiliation[grep("(CIBIO/InBIO)",df$affiliation)] <- "Research Center in Biodiversity and Genetic Resources (CIBIO,InBIO), University of Evora, 7002 554 Evora, Portugal"
df$affiliation[grep("INIBIOMA",df$affiliation)] <- "INIBIOMA (CONICET, Universidad Nacional del Comahue) Bariloche, Rio Negro, Argentina"
df$affiliation[grep("Agroecology and Environment, Agroscope, Reckenholzstrasse 191",df$affiliation)] <- "Agroecology and Environment, Agroscope, Reckenholzstrasse 191, Zurich, CH 8046 Switzerland"
df$affiliation[grep("National Institute for Research in the Amazon",df$affiliation)] <- "National Institute for Research in the Amazon (INPA), Coordination of Research in Biodiversity (COBIO), 2936 Andre Araujo Ave, Petropolis, 69067 375 Manaus, AM, Brazil"

df$affiliation <- gsub("BETA Technological Center, University of Vic–University of Central Catalonia, Carrer de la Laura 13, 08500 Vic, Catalonia, Spain",
                       "BETA Technological Center, University of Vic, University of Central Catalonia, Carrer de la Laura 13, 08500 Vic, Catalonia, Spain", df$affiliation)

df$funding <- gsub("\u00A0", " ", df$funding, fixed = TRUE)
df$funding <- gsub("\u3000"," ", df$funding)
df$funding <- gsub("“", "", df$funding, fixed = TRUE)
df$funding <- gsub("”","", df$funding)
df$funding <- gsub("’","", df$funding)
df$funding <- gsub("‘","", df$funding)
df$funding <- gsub(" – "," ", df$funding)
df$funding <- gsub("\n","", df$funding)
df$funding <- gsub("/"," ", df$funding)
df$funding <- gsub(" - "," ", df$funding)
df$funding <- gsub("-"," ", df$funding)
df$funding <- gsub("–"," ", df$funding)
df$funding <- gsub("º "," ", df$funding)

tools::showNonASCII(df$study_id) %>% unique()
tools::showNonASCII(df$name) %>% unique()
tools::showNonASCII(df$affiliation) %>% unique()
tools::showNonASCII(df$email) %>% unique()
tools::showNonASCII(df$role) %>% unique()
tools::showNonASCII(df$funding) %>% unique()


data_ownership$study_id <- df$study_id
data_ownership$name <- df$name
data_ownership$affiliation <- df$affiliation
data_ownership$email <- df$email
data_ownership$role <- df$role
data_ownership$funding <- df$funding

###################
#Uniform values in role
data_ownership$role %>% unique()

data_ownership$role[data_ownership$role=="Co-author"] <- "Co-author/Co-owner"
data_ownership$role[data_ownership$role=="Lead author/Project coordinator"] <- "Lead author/Corresponding author"
data_ownership$role[data_ownership$role=="Corresponding author"] <- "Lead author/Project coordinator"
data_ownership$role[data_ownership$role=="coauthor"] <- "Co-author/Co-owner"
data_ownership$role[data_ownership$role=="Project principal investigator"] <- "Co-author/Co-owner"
data_ownership$role[data_ownership$role=="Co-author/PI"] <- "Co-author/Co-owner"
data_ownership$role[data_ownership$role=="Sandra.Lindstrom@hushallningssallskapet.se"] <- "Lead author/Corresponding author"
data_ownership$role[data_ownership$role=="Coauthor"] <- "Co-author/Co-owner"
data_ownership$role[data_ownership$role=="Lead author/Project coordinator"] <- "Lead author/Corresponding author"


data_ownership$role %>% unique() #OK

######
# Update Jens A Trif. Prat.

data_jens <- data_ownership %>% filter(study_id==
                                         "Jens_Astrom_Trifolium_pratense_Norway_several_years")

data_jens <- bind_rows(data_jens,data_jens) %>% mutate(study_id=
                                                         c("Jens_Astrom_Trifolium_pratense_Norway_2013",
                                                           "Jens_Astrom_Trifolium_pratense_Norway_2014"))

data_ownership <- data_ownership %>% filter(study_id!=
                                              "Jens_Astrom_Trifolium_pratense_Norway_several_years") %>%
  bind_rows(data_jens)

######
# Save data ownership info
write_csv(data_ownership, "Processing_files/Datasets_storage/data_ownership_first_compilation.csv")
