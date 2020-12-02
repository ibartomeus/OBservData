
# This script estimates abunadance and sampling effort
# for the study in the folder "Your_new_study"

# We only use pantraps if there are no other alternative methods

if(("pan trap, bee bowl, blue vane trap, pitfall" %in% methods_abundance) &
   (length(methods_abundance > 1))){

    methods_abundance <-
      methods_abundance[methods_abundance !=
                          "pan trap, bee bowl, blue vane trap, pitfall"]
  }


# Estimating abundance with the methods in the variable "methods_abundance"

abundance_aux <- data.insect %>% filter(sampling_method %in% methods_abundance) %>%
  select(study_id,site_id,guild,abundance) %>%
  group_by(study_id,site_id,guild) %>% count(wt=abundance) %>%
  spread(key=guild, value=n)

all_guilds <- c("honeybees", "bumblebees", "other_wild_bees",
                "syrphids", "humbleflies", "other_flies",
                "beetles", "non_bee_hymenoptera", "lepidoptera", "other",
                "total")

abundance_aux_col <- c("study_id", "site_id", all_guilds)

new_col <- abundance_aux_col[!abundance_aux_col %in% names(abundance_aux)]

for (i in 1:length(new_col)){
  abundance_aux[,new_col[i]] <- 0
}

abundance_aux[is.na(abundance_aux)] <- 0
abundance_aux$total <- rowSums(abundance_aux[,c(3:ncol(abundance_aux))])

# Estimating sampling effort

sampling_aux <- data.insect %>% filter(sampling_method %in% methods_abundance) %>%
  select(study_id,site_id,total_sampled_area,total_sampled_time) %>%
  group_by(study_id,site_id) %>% summarise_all(mean) %>%
  rename(area = total_sampled_area, time = total_sampled_time)

# Adding the estimated data

data.site <- data.site %>% left_join(abundance_aux, by = c("study_id","site_id"))
data.site <- data.site %>% left_join(sampling_aux, by = c("study_id","site_id"))

data.site <- data.site %>% mutate(
  sampling_abundance = paste0(methods_abundance,collapse = "+"),
  abundance = data.site$total,
  ab_honeybee = data.site$honeybees,
  ab_bombus = data.site$bumblebees,
  ab_wildbees = data.site$other_wild_bees,
  ab_syrphids = data.site$syrphids,
  ab_humbleflies= data.site$humbleflies,
  ab_other_flies= data.site$other_flies,
  ab_beetles=data.site$beetles,
  ab_lepidoptera=data.site$lepidoptera,
  ab_nonbee_hymenoptera=data.site$non_bee_hymenoptera,
  ab_others = data.site$other,
  total_sampled_area = data.site$area,
  total_sampled_time = data.site$time
  ) %>% select(
    -total,-honeybees,-bumblebees,-other_wild_bees,-syrphids,
    -humbleflies, -other_flies,-beetles,-lepidoptera,
    -non_bee_hymenoptera,-other,-area,-time
  )

# clean auxiliary varibles
rm(abundance_aux,sampling_aux,new_col,abundance_aux_col,all_guilds,i)

