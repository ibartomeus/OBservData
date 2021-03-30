
library(EML)
library(tidyverse)

#schema
# - eml
#   - dataset
#   - creator
#   - title
#   - publisher
#   - pubDate
#   - keywords
#   - abstract
#   - intellectualRights
#   - contact
#   - methods
#   - coverage
#     - geographicCoverage
#     - temporalCoverage
#     - taxonomicCoverage
#   - dataTable
#     - entityName
#     - entityDescription
#     - physical
#     - attributeList

field_data <- read_csv("Final_Data/CropPol_field_level_data.csv")

# Extract lomg/lat

longitude <- field_data$longitude %>% unique()
longitude <- longitude[!is.na(longitude)]
min_X <- min(longitude)
max_X <- max(longitude)
latitude <- field_data$latitude %>% unique()
latitude <- latitude[!is.na(latitude)]
min_Y <- min(latitude)
max_Y <- max(latitude)

# extract altitude
#
# long_lat <- field_data[,c("longitude","latitude")] %>% filter(!is.na(longitude),!is.na(latitude)) %>% unique()
# options(geonamesUsername="alfonso_allen")
# options(geonamesHost="api.geonames.org")
# library(geonames)
# GNsrtm3(long_lat[1,2] %>% pull(),long_lat[1,1]%>% pull())


years <- as.numeric(field_data$sampling_year) %>% unique()
years <- years[!is.na(years)]
min_year <- min(years)
max_year <- max(years)

# keywords
#Pollination, crop production, agricultural management, pollinator 1 biodiversity, bees, flower visiting insects

keywordSet <- list(
  list(
    keywordThesaurus = "Selected by authors",
    keyword = list("Pollination",
    "Crop production", "Agricultural management", "Pollinator", "Biodiversity",
    "Bees", "Flower visiting insects")
    ))


#setwd("C:/Users/USUARIO/Desktop/OBservData/")
abstract <- set_TextType("Manuscript/abstract.docx")

#more
geographicDescription <- "Global"
coverage <-
  set_coverage(begin = min_year, end = max_year, #update
               geographicDescription = geographicDescription,
               west = min_X, east = max_X, #update
               north = min_Y, south = max_Y) #update

ignasi <- eml$creator(
  individualName = eml$individualName(
    givenName = "Ignasi",
    surName = "Bartomeus"),
  electronicMailAddress = "nacho.bartomeus@gmail.com")

EBD_address <- eml$address(
  deliveryPoint = "Estación Biológica de Doñana",
  city = "Seville",
  administrativeArea = "Seville",
  postalCode = "41092",
  country = "Spain")


contact <-
  list(
    individualName = ignasi$individualName,
    electronicMailAddress = ignasi$electronicMailAddress,
    address = EBD_address,
    organizationName = "Estación Biológica de Doñana, Consejo Superior de Investigaciones Científicas")


CP_eml <- eml$eml(
  packageId = uuid::UUIDgenerate(),
  system = "uuid",
  dataset = eml$dataset(
    title = "CropPol: a dynamic, open and global database on crop pollination",
    creator = ignasi,
    pubDate = "2021",
    intellectualRights = "CC-by",
    abstract = abstract,
    keywordSet = keywordSet,
    coverage = coverage,
    contact = contact
    #methods = methods,
    # dataTable = eml$dataTable(
    #   entityName = "clean_data.csv",
    #   entityDescription = "",
    #   physical = physical,
    #   attributeList = attributeList)
  ))


eml_validate(CP_eml)
write_eml(CP_eml, "Metadata/docs/EML_CP.xml")
