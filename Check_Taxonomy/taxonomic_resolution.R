
library(tidyverse)
library(openxlsx)

# Load taxon (DAINESE)
dainese_taxon <- read_csv("taxon_DAINESE_corrected.csv") %>% unique() %>%
  rename(pollinator=Organism_ID)

# Load taxon Rader
rader_taxon <- read_csv("taxon_table_Rader.csv") %>%
  dplyr::select(-matched_name)

# Load taxon Silvia
silvia_taxon <- read_csv("taxon_Silvia_simple.csv")

# Load other taxon 

other_taxon <- read_csv("taxon_other_studies.csv")


# Load final insect sampling
insect_sampling <- read_csv("../Final_Data/FINAL_sampling_data_V0p2.csv")

insect_sampling %>% filter(study_id %in% 
                             c(dainese_taxon$study_id)) #16,791 entries


add_dainese <- insect_sampling %>%
  left_join(dainese_taxon,by=c("study_id","sampling_method","pollinator")) 

# Fix You lepidoptera families

add_dainese$rank[add_dainese$study_id=="Yi_Zou_Brassica_napus_China_2015" &
                   is.na(add_dainese$rank)] <- "family"


add_dainese %>% filter(is.na(rank)) #28,752 entries need resolution


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


thesaurus_taxon_guild <- bind_rows(add_dainese_rader,add_Silvia) %>%
  select(study_id, site_id, sampling_method,pollinator, guild, rank,notes) %>%
  rename(identified_to=rank) %>% unique()

thesaurus_taxon_guild %>% select(study_id) %>% unique()
thesaurus_taxon_guild %>% select(site_id) %>% unique()
thesaurus_taxon_guild %>% select(sampling_method) %>% unique()
thesaurus_taxon_guild %>% select(pollinator) %>% unique()
thesaurus_taxon_guild %>% select(identified_to) %>% unique()
insect_sampling %>% select(abundance) %>% unique()
insect_sampling %>% select(total_sampled_area) %>% unique()
insect_sampling %>% filter(!is.na(total_sampled_area)) %>%
  select(total_sampled_area)%>% min()
insect_sampling %>% select(total_sampled_time) %>% unique()
insect_sampling %>% filter(!is.na(total_sampled_time)) %>%
  select(total_sampled_time)%>% min()
insect_sampling %>% select(total_sampled_flowers) %>% unique()
insect_sampling %>% filter(!is.na(total_sampled_flowers)) %>%
  select(total_sampled_flowers)%>% min()
insect_sampling %>% select(Description) %>% unique()
insect_sampling$Description[grep("within one",insect_sampling$Description,ignore.case = T)]

# write_csv(thesaurus_taxon_guild,"thesaurus_taxon_guild.csv")

insect_sampling_full <- bind_rows(add_dainese_rader,add_Silvia)
insect_sampling_full_mod <- insect_sampling_full 


# Here begins the relabelling


insect_sampling_full$rank[insect_sampling_full$rank=="guild" &
                            insect_sampling_full$pollinator %in%
                            c("Fly","Beetle","Spider")] <- "order"

insect_sampling_full$rank[insect_sampling_full$rank=="guild" &
                            insect_sampling_full$pollinator %in%
                            c("Butterfly","Wild bee")] <- "superfamily"

insect_sampling_full$rank[insect_sampling_full$rank=="guild" &
                            insect_sampling_full$pollinator %in%
                            c("Wasp","Hoverfly","Ladybug")] <- "family"


insect_sampling_full$rank[insect_sampling_full$rank=="group" &
                            insect_sampling_full$pollinator %in%
                            c("Lepidoptera","Araneae",
                              "Parasitica","other.diptera",
                              "Heteropt","solitary.bee")] <- "order"

insect_sampling_full$rank[insect_sampling_full$rank=="group" &
                            insect_sampling_full$pollinator %in%
                            c("Aphids","other.syrphidae")] <- "family"

insect_sampling_full$rank[insect_sampling_full$rank=="group" &
                            insect_sampling_full$pollinator %in%
                            c("Gastropoda")] <- "class"


insect_sampling_full$rank[insect_sampling_full$rank=="species or morphospecies" &
                            insect_sampling_full$pollinator %in%
                            c("Apis mellifera L.",
                              "Astylus atromaculatus Blanchard *",
                              "Dioxyna sororcula",
                              "“Lagria” cf. aeneipennis Fåhraeus",
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



insect_sampling_full$rank[insect_sampling_full$rank=="species or morphospecies" &
                            insect_sampling_full$pollinator %in%
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

  
insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("family","Family",
                              "subfamily","Subfamily",
                              "superfamily","Superfamily")] <- "family/subfamily/superfamily"

insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("gener","subgenus","genera",
                              "Genera","genere","genus","Genus",
                              "tribe","genus/subgenus",
                              "genus/subgenus/tribe")] <- "genus/subgenus/tribe" 


insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("morpho-group",
                              "morphoespecies",
                              "morphospecies" ,"morpospecies",
                              "Genera (2 species not differentiated)")] <- "morphospecies"  

insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("species","Species","species complex",
                            "species group","species level","specie",
                            "species_undescribed",
                            "species or morphospecies")] <- "species"

insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("suborder","order","Order",
                              "order/suborder")] <- "order/suborder"

insect_sampling_full$rank[insect_sampling_full$rank %in% 
                            c("other","others","Unknown","Others",
                              "other/unknown")] <- "other/unknown"

insect_sampling_full$rank[is.na(insect_sampling_full$rank)] <- "other/unknown"

# Results 
insect_sampling_full %>% group_by(rank) %>% count()











