
# This script estimates richness for the study in the folder "Your_new_study"

# We only use pantraps if there are no other alternative methods

if("pan trap, bee bowl, blue vane trap, pitfall" %in% methods_richness_type){

  if(length(methods_richness_type==1)){
    only_pantraps <- T
  }else{
    only_pantraps <- F

    index_pantraps_r <- which(methods_richness == "pan trap, bee bowl, blue vane trap, pitfall")

    methods_richness <- methods_richness[-index_pantraps_r]

    methods_richness_type <- methods_richness_type[-index_pantraps_r]

  }

}else{
  only_pantraps <- F
}


# Estimating abundance with the methods in the variable "methods_richness"

abundance_field <- data.insect %>%
  filter(sampling_method %in% methods_richness,
         !is.na(guild)) %>%
  select(study_id, site_id,pollinator,abundance) %>%
  group_by(study_id,site_id,pollinator) %>% count(wt=abundance)

abundance_field <- abundance_field %>% spread(key=pollinator,value=n)

# abundance_field <- data_raw[,c(3,42:64)] %>%
#   rename(site_id=Site_name) %>% select(-Date,-Time,-Insects,-'no_flowers.(inflorescences)')

abundance_field[is.na(abundance_field)] <- 0
abundance_field$r_obser <-  0
abundance_field$r_chao <-  0

for (i in 1:nrow(abundance_field)) {
  x <- as.numeric(abundance_field[i,3:(ncol(abundance_field)-2)])
  chao  <-  ChaoRichness(x, datatype = "abundance", conf = 0.95)
  abundance_field$r_obser[i] <-  chao$Observed
  abundance_field$r_chao[i] <-  chao$Estimator
}

# Estimating richness restriction

data.site$all_bees <-  data.site$abundance -
  (data.site$ab_honeybee + data.site$ab_bombus + data.site$ab_wildbees)

if(any(data.site$all_bees > 0)){
  data.site$richness_restriction <- "all visitors considered"
}else{
  data.site$richness_restriction <- "only bees"
}

data.site <- data.site %>% select(-all_bees)

# Estimating taxonomic resolution

ab_species_morph <- length(grep("specie",data.insect$identified_to))

percentage_species_morphos <- ab_species_morph / nrow(data.insect)

# Transposing estimations

richness_aux <- abundance_field %>% select(study_id,site_id,r_obser,r_chao) %>%
  mutate(other_richness_estimator_method_aux="Chao1")

if (percentage_species_morphos < 0.75 | only_pantraps==TRUE ){
  richness_aux[,3:ncol(richness_aux)] <- NA
}

data.site <- data.site %>% left_join(richness_aux,by=c("study_id","site_id")) %>%
  mutate(
    sampling_richness = paste0(methods_richness_type,collapse = "+"),
    observed_pollinator_richness=r_obser,
    other_pollinator_richness=r_chao,
    other_richness_estimator_method=other_richness_estimator_method_aux) %>%
  select(-r_obser,-r_chao,-other_richness_estimator_method_aux)

if(only_pantraps==TRUE){
  data.site$notes <- paste(data.site$notes,
                          "Information on floral visitors was obtained from pan-trap records. Richness can be calculated by using the sampling records for this study_id.",
                          sep = ". ")
}

# clean auxiliary varibles
rm(only_pantraps,percentage_species_morphos,x,i,ab_species_morph,richness_aux,
   chao,abundance_field)
