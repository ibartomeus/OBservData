

library(tidyverse)

DAINESE <- read_csv("organism_guild_DAINESE.csv")
DAINESE <- as.tibble(DAINESE) %>% mutate(Guild=NA)%>% rename(Organism_ID=OrganismID)

ROMINA <- read_csv("organism_guild_ROMINA.csv")
ROMINA <- as.tibble(ROMINA) %>% mutate(Family=NA)
ROMINA <- ROMINA %>% rename(Organism_ID=organism_ID,Guild=organism_guild)

GARIBALDI <- read_csv("organism_guild_GARIBALDI_2015.csv")
GARIBALDI <- as.tibble(GARIBALDI) %>% mutate(Family=NA)
GARIBALDI <- GARIBALDI %>%  rename(Organism_ID=organism_ID,Guild=organism_guild)

familias_list <- bind_rows(DAINESE,ROMINA,GARIBALDI) %>% unique() %>% arrange(Organism_ID)

write_csv(familias_list, "organism_guild_META_STUDIES.csv")


#Apis Mellifera
familias_list$Guild[grepl("Mell",familias_list$Organism_ID,ignore.case = TRUE)] <- "honeybees"
familias_list$Guild[grepl("Melif",familias_list$Organism_ID,ignore.case = TRUE)] <- "honeybees"
familias_list$Guild[grepl("honey_bee",familias_list$Organism_ID,ignore.case = TRUE)] <- "honeybees"


#Bombus
familias_list$Guild[grepl("bombu",familias_list$Organism_ID,ignore.case = TRUE)] <- "bumblebees"
familias_list$Guild[grepl("bomu",familias_list$Organism_ID,ignore.case = TRUE)] <- "bumblebees"
familias_list$Guild[grepl("B. ",familias_list$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
familias_list$Guild[grepl("B_",familias_list$Organism_ID,ignore.case = FALSE)] <- "bumblebees"
familias_list$Guild[grepl("bumble_bee",familias_list$Organism_ID,ignore.case = TRUE)] <- "bumblebees"


#humbleflies
familias_list$Guild[grepl("bombyli",familias_list$Family,ignore.case = TRUE)] <- "humbleflies"

#Butterflies and moths
familias_list$Guild[grepl("lepid",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("moth",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Arctiidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Pieridae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Pieris",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Lycaenidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Papilionidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Catopsilia",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Nymphalidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Danaus",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Euploea",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Hypolimnas",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Lepidoptera",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Hesperiidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Macroglossum",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Sphingidae",familias_list$Family,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Mariposa",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Pararge",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Parnara",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Pelopidas",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Polytremis",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Utetheisa",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Vanessa",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Zizina_labradus",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"


#wild_bees
familias_list$Guild[grepl("Halictidae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Halictidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Pseudapis",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Megalopta",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Lipotriches",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Lasioglossum",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Colletidae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Hylaeus",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Hylaeinae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Hylae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Megachilidae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apoidea",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apoidea",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apoidae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Lithurg",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Diadasia",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Anthophoridae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Andrenidae",familias_list$Family,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Megachile",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Osmia",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Panurginus",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Sphecodes",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Wild bee",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Wild_bee",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"


# Syrphids
familias_list$Guild[grepl("syrph",familias_list$Family,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("syrph",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("syrphidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("sir",familias_list$Family,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Cheilosia",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Eristalis",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Eumerus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Eupeodes",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Helophilus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Stratiomyiidae",familias_list$Family,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Stratiom",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Melanogaster",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Melanostoma",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Merodon",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Myathropa",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Neoascia",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Paragus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Parasyrphus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Parhelophilus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Phytomia",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Pipizella",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Platycheirus",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Rhingia",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Scaeva",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Senaspis",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Sphaerophoria",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Syritta",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Volucella",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"


# coleoptera
familias_list$Guild[grepl("coleopt",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("coleopt",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Apionidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Melyridae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Chrysomelidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Curculionidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Curculionoidea",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Bembidion",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Carabidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("beetle",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Nitidulidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Carpophilus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Coccinellidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Coccinellidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Coccinella",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Meligethes",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Haptoncus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Brachinus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Bruchidius",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Cantharidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Carabus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Cerambycidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Cerambycidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Scarabaeidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Cetoni",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Cyclocephala",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Decaria",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Dermestidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Exochomus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Gametis",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Harpalus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Lathrididae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Lycidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Macrocoma",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Macrodactylus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Malachiinae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Meligethies",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Monolepta",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Mordellinae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Nebria",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Nitulidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Oedemeridae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Parophonus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Pedinorrhina",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Poecilus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Pterostichus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Ripiphoridae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Staphylinidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Staphilinidae",familias_list$Family,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Trechus.quadristriatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"



#Ants
familias_list$Guild[grepl("formic",familias_list$Family,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("formic",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Anoplol",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Camponotus",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Lepisiota",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Tetramorium",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"


#Wasp
familias_list$Guild[grepl("wasp",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("vespid",familias_list$Family,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Oreumenes",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("vespid",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Vespula",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"



#Snails
familias_list$Guild[grepl("Gastropod",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"

#spider
familias_list$Guild[grepl("spider",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"


#Birds
familias_list$Guild[grepl("Bird",familias_list$Organism_ID,ignore.case = FALSE)] <- "other"
familias_list$Guild[grepl("Hummingbird",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"
familias_list$Guild[grepl("Curcumelidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"

#chinches
familias_list$Guild[grepl("Heteroptera",familias_list$Organism_ID,ignore.case = FALSE)] <- "other"
familias_list$Guild[grepl("Miridae",familias_list$Family,ignore.case = FALSE)] <- "other"
familias_list$Guild[grepl("Nezara",familias_list$Organism_ID,ignore.case = FALSE)] <- "other"



#Nacho's edits
#start with NA's

#test missing ones ----
familias_list %>% group_by(Guild)%>% count()

familias_list2 <- as.data.frame(familias_list)
familias_list2[which(is.na(familias_list2$Guild)),]

#New additions----
#si incluimos todas las apis como honeybees
familias_list$Guild[grepl("Apis",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "honeybees"

#syrphidos
familias_list$Guild[grepl("flower_fly",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "syrphids"
familias_list$Guild[grepl("Syrhipds",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "syrphids"


#wild bees por genero
familias_list$Guild[grepl("Abeja",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Andrena",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Amegilla",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Anthidium",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Anthophora",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Augochlora",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Augochloropsis",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Centris",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Ceratina",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Coelioxys",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Colletes",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Epeolus",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Eucera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Euglossa",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Habropoda",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Halictus",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Heriades",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Hoplitis",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Lasioslossum",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Leioproctus",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Eulaema",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Evylaeus",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Lassioglossum",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Macrogalea",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Manuelia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Melipona",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Meliponini",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Meliponula",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Melissodes",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Melissoptila",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Melitoma",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Nannotrigona",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("native_bee",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Nomada",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Nomia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("bee",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Paratrigona",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Partamona",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Plebeia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Ptilothrix",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Svastra",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Systropha",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Tetragonula",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Tetralonia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Tetraloniella",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Thyreus",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Trigona",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Xylocopa",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"


#other diptera.
familias_list$Guild[grepl("Asilidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Bembix",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Bibio",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Bibionidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Caliophoridae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Calliphora",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Chrysomya",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Chrysops",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Crane",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Diptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Drosophila",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("fly",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Fly",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Eristal",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Housefly",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Lucilia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Calliphoridae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Diptero",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Empididae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Empis",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Erythrocera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Lucillia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Mosca",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Musca",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Muscidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Neomyia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Neophryxe",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Pales",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Penthetria",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Plecia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Tachinidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Tipulidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"


#other hymenoptera.
familias_list$Guild[grepl("ant",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Ant",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Avispa",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Braconidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Campsomeriella",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Cerceris",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Crabronidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Hymenoptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera" #I think is a fine guess
familias_list$Guild[grepl("Hormiga",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Ichneumonidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Megacampsomeris",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Ichneumonidea",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Ichneumonoidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Ichneumonoidea",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Megascolia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Microgastrinae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Parasitica",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Pemphredon",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Polistes",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Polistinae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Parasitoids",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Pepsis",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Saw",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Scolia",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Scoliidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Sphecidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Symphyta",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Brachygastra",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Tachytes",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Tiphiidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"

#other
familias_list$Guild[grepl("Hemiptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("heteroptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Aphid",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Araneae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Heteropt",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Neuroptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Odonata",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Orthoptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Thysanoptera",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"



#beetles
familias_list$Guild[grepl("Ladybug",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"
familias_list$Guild[grepl("weevil",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"
familias_list$Guild[grepl("Weevil",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"


#humbleflies
familias_list$Guild[grepl("Bombilidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "humbleflies"

#lepidoptera
familias_list$Guild[grepl("Hesperidae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"
familias_list$Guild[grepl("Pieridae",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"



#going for families------
#order is important, as some families were wrong, and species need to be assigned first.
familias_list2 <- as.data.frame(familias_list)
familias_list2[which(is.na(familias_list2$Guild) & !is.na(familias_list2$Family)),]
#check guild names are well spelled.

familias_list$Guild[grepl("Tenebrionidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"

familias_list$Guild[grepl("Agromyzidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Sarcophagidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Apidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Argidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Tenthredinidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Tabanidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Brachycera",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Bracronidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Sciaridae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Tephritidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Chalcididae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Chaldidoidea",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Stratiomyidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Chrysididae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Orthoptera",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Cyclorrhapha",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Anthomyiidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Dolichopodidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Asildae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Elateridae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"
familias_list$Guild[grepl("Empididae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("EUCOILIDAE",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Hepialidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"
familias_list$Guild[grepl("Tachinidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Fanniidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Muscidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Melittidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_wild_bees"
familias_list$Guild[grepl("Melolonthidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "beetles"
familias_list$Guild[grepl("Nematocera",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Stratiomyidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Tipulidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Calliphoridae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Plecoptera",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Plutellidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"
familias_list$Guild[grepl("POMPILIDAE",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Pompillidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Chrysopidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Calliophoridae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Sacrophagidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Crabronidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Sciomyzidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Pentatomoidea",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Berytidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
familias_list$Guild[grepl("Rhiniidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_diptera"
familias_list$Guild[grepl("Crabronidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other_hymenoptera"
familias_list$Guild[grepl("Hesperidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"
familias_list$Guild[grepl("Noctuidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"
familias_list$Guild[grepl("Pyralidae",familias_list$Family,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "lepidoptera"

#recap----
familias_list %>% group_by(Guild)%>% count() #146 missing; 

familias_list2 <- as.data.frame(familias_list)
familias_list2[which(is.na(familias_list2$Guild)),]


#Alfonso's new edit-----

# relabel guilds
familias_list$Guild[familias_list$Guild=="other_diptera"] <- "other_flies"
familias_list$Guild[familias_list$Guild=="other_hymenoptera"] <- "non_bee_hymenoptera"

#beettles
familias_list$Guild[grepl("Abax.parallelepipedus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Adalia_bipunctata",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Agonum.muelleri",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Agonum.sexpunctatum",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.aenea",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.convexior",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.familiaris",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.montivaga",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.ovata",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.plebeja",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Amara.similata",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Anchmenus.dorsalis",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Anisodactylus.binotatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Anisodactylus.signatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Apionidae_sp1",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Astylus_atromaculatus_Blanchard",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Badister.bullatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Baris_cfatrocoerulea_Boheman_1844",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Chrysomelidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Clivina.collaris",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Clivina.fossor",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Coccinelidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Demetrias.atricapillus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
#? Elateratidae
familias_list$Guild[grepl("Harmonia_axyridis_I_158",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Hippodamia_variegata_Goeze",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Lagria_cf_aeneipennis_Fahraeus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Loricara.pilicornis",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Melyridae_sp1",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Notiophilus.aestuans",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Notiophilus.palustris",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Pagurodactylus_sp2",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Platynus.assimilis",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Stenolophus.teutonus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Stomis.pumicatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Tachys.bistriatus",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Trichotichnus.nitens",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"
familias_list$Guild[grepl("Alleculinae_sp1",familias_list$Organism_ID,ignore.case = TRUE)] <- "beetles"

#non_bee_hymenoptera
familias_list$Guild[grepl("Arge_nipponensis_Rohwer_1910",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Athalia_infumata_Marlatt_1898",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Athalia_rosae_ruficornis_Jakovrev_1888",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Belanogaster_sp_",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Chalcidoidea_sp",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Chrysidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Dolerus_similis_japonicus_Kirby_1882",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Eumenes_sp",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
#? Eumeniinae_sp.
familias_list$Guild[grepl("Pompilidae_cf",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
familias_list$Guild[grepl("Pompilidae_sp1_hipolitodataset",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"


#other_flies
familias_list$Guild[grepl("Agromyzidae_sp1",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Amobia_sp",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Bibo_hortulans
#?	Bibo_marci
#?	Bibo_varipes
familias_list$Guild[grepl("Boettcherisca_peregrina",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Bradysia_sp.",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#?Chironomidae_sp.
familias_list$Guild[grepl("Chloromyia formosa",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Culcidae.sp.
#?	Culicidae_sp1_hipolitodataset
#?	Culicidae_sp2_hiploitodataset
#?	Culicidae_sp3_hipolitodataset
#?	Culicidae_sp4_hipolitodataset
familias_list$Guild[grepl("Delia_platura",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Dilophilus_febrilis
#? Dilophus_nigrostigma
familias_list$Guild[grepl("Dioxyna_sororcula",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Dolichopodidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Exorista_Podotachin_sorbillans",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Isoconia_haematopota_cf",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Myorhina_uniseta",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Nematocera_sp.",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Onesia_nartshukae_complex",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Otitidae
familias_list$Guild[grepl("Oxysarcodexia_varia",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Parasarcophaga_albiceps",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Parasarcophaga_similis",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Parasarcophaga_unguitigris",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Pollenia_pseudorudis",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Proscissio_sp.",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Sarcophagidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Scaptia_sp.
familias_list$Guild[grepl("Stomorhina_obsoleta",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Tachnidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Tanypezidae
#? Tephritidae
familias_list$Guild[grepl("Tipula_sp.",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
familias_list$Guild[grepl("Zenillia_albipila",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#? Scatophagidae


#humbleflies
familias_list$Guild[grepl("Bombylidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "humbleflies"

#syrphids 
#????Stratiomyidae Syrphid or other_flies
familias_list$Guild[grepl("Allograpta_calopus_talvez",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Chrysotoxum cautum",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Chrysotoxum_cautum",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Epistrophe.flava",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Epistrophe_nitidicollis",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
#? Melangyna_cincta
#? Melangyna_novaezelandiae
#? Milichiidae
#? Odontomyia_sp.
#? Salpinigaster_sp
familias_list$Guild[grepl("Tropidia_scita",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
familias_list$Guild[grepl("Xylota_segnis",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"


#other_wild_bees
familias_list$Guild[grepl("Apis cerana",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apis_cerana",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apis_dorsata",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Apis_florea",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Braunsapis sp.",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Braunsapis_picitarsus",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Ancyloscelis sp.2",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Ancyloscelis sp1",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Ceratinidia cognata",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Ceratinidia sp. 1",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Dialictus af. opacus",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
familias_list$Guild[grepl("Dialictus_12",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
#? Euaspis abdominalis
familias_list$Guild[grepl("Exomalopsis analis",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
#? Hapropoda sp.
#? Pachymelus conspicuus
familias_list$Guild[grepl("Plebeina hildebrandti",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_wild_bees"
#?T. corvina
#?T. nigerrima

#lepidoptera
familias_list$Guild[grepl("Nyctemera_annulata",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Orocrambus_flexuosellus",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Pirpinto amarillo",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
familias_list$Guild[grepl("Precis_iphita",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"
#? Tineoidea_sp.
familias_list$Guild[grepl("Zizina_labradus",familias_list$Organism_ID,ignore.case = TRUE)] <- "lepidoptera"


#other
familias_list$Guild[grepl("Glaucias_amyoti",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"
#? Ligaidae
familias_list$Guild[grepl("other",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
#? Lygaeidae_sp.
#? Micromus._tasmaniae
#? Phasmatodea_sp.
familias_list$Guild[grepl("shield_bug",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"
familias_list$Guild[grepl("Tingidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"
familias_list$Guild[grepl("tiny insect",familias_list$Organism_ID,ignore.case = TRUE)] <- "other"
familias_list$Guild[grepl("Unknown",familias_list$Organism_ID,ignore.case = TRUE) &
                      is.na(familias_list$Guild)] <- "other"
#? Zygoptera_sp.

# Corrections for other and unknow

#familias_list$Guild[grepl("Other Coleoptera",familias_list$Organism_ID,ignore.case = TRUE)] <- 	"beetles"	
#familias_list$Guild[grepl("Other Diptera",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#familias_list$Guild[grepl("Other Hymenoptera",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
#familias_list$Guild[grepl("other  bee",familias_list$Organism_ID,ignore.case = TRUE)] <-  "other_wild_bees"
#familias_list$Guild[grepl("Other solitary bee",familias_list$Organism_ID,ignore.case = TRUE)] <-  "other_wild_bees"
#familias_list$Guild[grepl("Other Solitary wasp",familias_list$Organism_ID,ignore.case = TRUE)] <- "non_bee_hymenoptera"
#familias_list$Guild[grepl("other.diptera",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#familias_list$Guild[grepl("other.syrphidae",familias_list$Organism_ID,ignore.case = TRUE)] <- "syrphids"
#familias_list$Guild[grepl("Other_Dipterans",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"
#familias_list$Guild[grepl("other_fly",familias_list$Organism_ID,ignore.case = TRUE)] <- "other_flies"


#familias_list$Guild[grepl("Coleoptera unknown or unidentified",familias_list$Organism_ID,ignore.case = TRUE)] <-  "beetles"
#familias_list$Guild[grepl("Diptera unknown or unidentified",familias_list$Organism_ID,ignore.case = TRUE)] <-  "other_flies"
#familias_list$Guild[grepl("Hoverfly unknown or unidentified",familias_list$Organism_ID,ignore.case = TRUE)] <-  "syrphids"
#familias_list$Guild[grepl("Solitary bee unknown or unidentified",familias_list$Organism_ID,ignore.case = TRUE)] <-  "other_wild_bees"
#familias_list$Guild[grepl("Bombus spp. unknown or unidentified",familias_list$Organism_ID,ignore.case = TRUE)] <-  "bumblebees"


familias_list$Guild[familias_list$Organism_ID=="Anasimyia_lineata"] <- "syrphids"
familias_list$Guild[familias_list$Organism_ID=="Bibo_hortulans"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Bibo_marci"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Bibo_varipes"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Chironomidae_sp."] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Culcidae.sp."] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Culicidae_sp1_hipolitodataset"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Culicidae_sp2_hiploitodataset"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Culicidae_sp3_hipolitodataset"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Culicidae_sp4_hipolitodataset"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Dilophilus_febrilis"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Dilophus_nigrostigma"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Elateratidae"] <- "beetles"
familias_list$Guild[familias_list$Organism_ID=="Eumeniinae_sp."] <- "non_bee_hymenoptera"
familias_list$Guild[familias_list$Organism_ID=="Hydrotaea_rostrata"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Lygaeidae_sp."] <- "other"
familias_list$Guild[familias_list$Organism_ID=="Melangyna_cincta"] <- "syrphids"
familias_list$Guild[familias_list$Organism_ID=="Melangyna_novaezelandiae"] <- "syrphids"
familias_list$Guild[familias_list$Organism_ID=="Micromus._tasmaniae"] <- "other"
familias_list$Guild[familias_list$Organism_ID=="Milichiidae"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Odontomyia_sp."] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Otitidae"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Phasmatodea_sp."] <- "other"
familias_list$Guild[familias_list$Organism_ID=="Protohystricia_alcis"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Salpinigaster_sp"] <- "syrphids"
familias_list$Guild[familias_list$Organism_ID=="Scaptia_sp."] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Scatophagidae"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Spilagona_melas"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Tanypezidae"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Tephritidae"] <- "other_flies"
familias_list$Guild[familias_list$Organism_ID=="Tineoidea_sp."] <- "lepidoptera"
familias_list$Guild[familias_list$Organism_ID=="Zygoptera_sp."] <- "other"



#recap----
familias_list %>% group_by(Guild)%>% count() #48 missing; 

# save list
write_csv(familias_list,"Table_organism_guild_META.csv")

