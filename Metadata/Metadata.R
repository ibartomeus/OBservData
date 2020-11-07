# THIS SCRIPT PREPARE METADATA WITH DATASPICE

library(dataspice)


#create_spice() # Creates metadata templates for us # Commented for security reasons!!!!

data_files <- list.files("C:/Users/USUARIO/Desktop/OBservData/Final_Data/",
                         pattern = "CropPol",
                         full.names = TRUE)

attributes_path <- file.path("data", "metadata", "attributes.csv")

#  this function can be applied over multiple files to populate the header names
data_files %>%
  purrr::map(~ prep_attributes(.x, attributes_path),
             attributes_path = attributes_path)


# Data were introduced by hand

edit_attributes() #opens Shiny for editing
edit_access() #opens an editable version of access.csv
edit_creators() #opens an editable version of creators.csv
edit_biblio() #opens an editable version of biblio.csv


write_spice() # Creates a JSON-LD document from our metadata

build_site()
