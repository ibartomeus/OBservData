
files_list <- list.files(path = "Processing_files/Datasets_processing/DAINESE 2019 DATABASE", pattern = "*.R", full.names = TRUE)

for (i in 1:length(files_list)){
  print(files_list[i])
  x <- readLines(files_list[i])
  x[x == 'setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")'] <- '# setwd("C:/Users/USUARIO/Desktop/OBservData/Datasets_storage")'
  x[x == 'setwd(dir_ini)'] <- '# setwd(dir_ini)'
  x[x == 'gild_list <- read_csv("C:/Users/USUARIO/Desktop/OBservData/Thesaurus_Pollinators/Table_organism_guild_META.csv")'] <- 'gild_list <- read_csv("Processing_files/Thesaurus_Pollinators/Table_organism_guild_META.csv")'

  write(x, files_list[i])
}

