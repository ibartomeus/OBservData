
# This script groups the values of variable "identified_to" in the file
# CropPol_sampling_data.csv

library(tidyverse)

# Load data

final_insect_sampling <- 
  "CropPol_sampling_data.csv"

final_insect_sampling_data <- read.csv(final_insect_sampling,encoding = "WINDOWS-1252",stringsAsFactors = F)
final_insect_sampling_data <- as_tibble(final_insect_sampling_data)
#######################

# Study with no "identified_to" info
final_insect_sampling_data %>% filter(is.na(identified_to)) %>% unique()
# The anomalous record in David_Kleijn_Allium_porrum_Italy_2012

########################
# Here begins the relabelling


final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="guild" &
                            final_insect_sampling_data$pollinator %in%
                            c("Fly","Beetle","Spider")] <- "order"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="guild" &
                            final_insect_sampling_data$pollinator %in%
                            c("Butterfly","Wild bee")] <- "superfamily"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="guild" &
                            final_insect_sampling_data$pollinator %in%
                            c("Wasp","Hoverfly","Ladybug")] <- "family"


final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="group" &
                            final_insect_sampling_data$pollinator %in%
                            c("Lepidoptera","Araneae",
                              "Parasitica","other.diptera",
                              "Heteropt","solitary.bee")] <- "order"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="group" &
                            final_insect_sampling_data$pollinator %in%
                            c("Aphids","other.syrphidae")] <- "family"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="group" &
                            final_insect_sampling_data$pollinator %in%
                            c("Gastropoda")] <- "class"


final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="species or morphospecies" &
                            final_insect_sampling_data$pollinator %in%
                            c("Apis mellifera L.",
                              "Astylus atromaculatus Blanchard *",
                              "Dioxyna sororcula",
                              "?Lagria? cf. aeneipennis F?hraeus",
                              "Hypolimnas misippus",
                              "Baris cf. atrocoerulea (Boheman, 1844)",
                              "Junonia oenone",
                              "Cynthia cardui",
                              "Eumerus obliquus",
                              "Macroglossum trochilus (Hubner)",
                              "Megachile frontalis cf.",
                              "Utetheisa pulchella",
                              "Betasyrphus adliagatus",
                              "Eudalaca exul cf",
                              "Senaspis haemorrhoa",
                              "Monolepta citrinella Jacoby",
                              "Xylocopa inconstans",
                              "Amata cerbera L.",
                              "Belenois thysa",
                              "Junonia hierta",
                              "Apis mellifera",
                              "Betassyrphus adligatus",
                              "Ischiodon aegyptius",
                              "Paragus longiventris",
                              "Scymnus cf. capicola Casey, 1899",
                              "Rhyncomya forcipata")] <- "species"



final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to=="species or morphospecies" &
                            final_insect_sampling_data$pollinator %in%
                            c("Carpophilus sp.",
                              "Heteroptera sp.",
                              "Lasioglossum sp.",
                              "Sarcophagidae sp.",
                              "Syrphidae sp1",
                              "Caliophoridae sp.",
                              "Melyridae sp1",
                              "Lepidoptera white",
                              "Vespidae sp.",
                              "Decaria sp.",
                              "Hesperidae sp2",
                              "Heteroptera sp1",
                              "moth sp3",
                              "Tetraloniella ? apicalis (Friese)",
                              "Syrphidae sp20",
                              "moth sp5",
                              "Lathrididae sp.",
                              "Lepidoptera spY",
                              "Diptera spX",
                              "Coleoptera sp1",
                              "Coleoptera sp5",
                              "Meligethes sp.",
                              "Ceratina sp. (Ceratina lunata Friese or Ceratina moerenhouti Vachal)",
                              "Empididae sp1",
                              "Empididae sp2",
                              "Syrphidae sp2",
                              "Eucoilidae sp1",
                              "Cratocentrus  sp",
                              "Lepidoptera sp.",
                              "Mordellinae sp1",
                              "Xylocopa sp.",
                              "Malachiinae sp1",
                              "Diptera sp1")] <- "morphospecies"


final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("family","Family",
                              "subfamily","Subfamily",
                              "superfamily","Superfamily")] <- "family/subfamily/superfamily"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("gener","subgenus","genera",
                              "Genera","genere","genus","Genus",
                              "tribe","genus/subgenus",
                              "genus/subgenus/tribe")] <- "genus/subgenus/tribe" 


final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("morpho-group",
                              "morphoespecies",
                              "morphospecies" ,"morpospecies",
                              "Genera (2 species not differentiated)")] <- "morphospecies"  

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("species","Species","species complex",
                              "species group","species level","specie",
                              "species_undescribed",
                              "species or morphospecies")] <- "species"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("suborder","order","Order",
                              "order/suborder")] <- "order/suborder"

final_insect_sampling_data$identified_to[final_insect_sampling_data$identified_to %in% 
                            c("other","others","Unknown","Others",
                              "other/unknown")] <- "other/unknown"

final_insect_sampling_data$identified_to[is.na(final_insect_sampling_data$identified_to)] <- "other/unknown"

# Results 
final_insect_sampling_data %>% group_by(identified_to) %>% count()
