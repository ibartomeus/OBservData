
extra_variables <- read_excel("Final_Data/Supporting_files/Dynamic_paper_extra_variables/field_data new columns.xlsx")

field_level_data_for_dynamic_web <- FINAL_field_level_data_filt %>%
  left_join(extra_variables, by = "study_id")

field_level_data_for_dynamic_web_reordered <- field_level_data_for_dynamic_web[,c(
  "study_id","study_id2","site_id","crop","variety","management","country",
  "latitude","longitude","X_UTM","Y_UTM","zone_UTM","sampling_start_month",
  "sampling_end_month","sampling_year","field_size","yield","yield_units",
  "yield2","yield2_units","yield_treatments_no_pollinators",
  "yield_treatments_pollen_supplement","yield_treatments_no_pollinators2",
  "yield_treatments_pollen_supplement2","fruits_per_plant","fruit_weight",
  "plant_density","seeds_per_fruit","seeds_per_plant","seed_weight",
  "taxa_recorded","use_visits_or_abundance","sampling_richness",
  "observed_pollinator_richness","other_pollinator_richness",
  "other_richness_estimator_method","richness_restriction","sampling_abundance",
  "abundance","ab_honeybee","ab_bombus","ab_wildbees","ab_syrphids",
  "ab_humbleflies","ab_other_flies","ab_beetles","ab_lepidoptera",
  "ab_nonbee_hymenoptera","ab_others","total_sampled_area","total_sampled_time",
  "sampling_visitation","visitation_rate_units","visitation_rate",
  "visit_honeybee","visit_bombus","visit_wildbees","visit_syrphids",
  "visit_humbleflies","visit_other_flies","visit_beetles","visit_lepidoptera",
  "visit_nonbee_hymenoptera","visit_others","Publication","Credit",
  "Email_contact","notes")
]

# Sanity check
field_level_data_for_dynamic_web_reordered %>% filter(is.na(study_id2))
field_level_data_for_dynamic_web_reordered %>% filter(is.na(taxa_recorded))
field_level_data_for_dynamic_web_reordered %>% filter(is.na(use_visits_or_abundance))
