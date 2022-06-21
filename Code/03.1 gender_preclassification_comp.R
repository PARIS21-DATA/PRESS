
# Call automatically the classification for different combinations of gender filter
files_gen_comb <- list.files(path = "./Data/Gender permutations/", pattern = "^df_crs")
files_gen_comb <- paste0("./Data/Gender permutations/", files_gen_comb)

# Initialize predicition data frame 
pred_all <- data.frame(text_id = as.numeric(), 
                       description = as.character(),
                       donorname = as.character())

for (gen_file in files_gen_comb[7]) {
  # Create identifier for naming files 
  gen_identifier <- str_replace_all(gen_file, "./Data/Gender permutations/df_crs_", "")
  gen_identifier <- str_replace_all(gen_identifier, ".rds", "")
  
  df_crs <- readRDS(gen_file)
  
  df_crs_original <- df_crs 
  df_crs <- df_crs_original %>%
    filter(is.na(description_comb) == FALSE) %>%
    select(text_id, description = description_comb, gender_filter = text_filter_gender, donorname) %>%
    mutate(language = cld2::detect_language(description)) %>% 
    filter(language == "en") %>%
    select(-language) %>%
    distinct()
  
  if (gen_file == files_gen_comb[1]) {
    pred <- df_crs %>%
      filter((gender_filter == FALSE | is.na(gender_filter))) %>%
      sample_n(size = floor(0.1 * n())) #use only frac_pred_set% to speed up for testing
    
    pred_for_all <- pred 
  }
  
  source("./Code/06.1 gender_classification_XGB_tmp.R")
  
  if (gen_file == files_gen_comb[1]) {
    pred_all <- pred_all %>%
      rbind(pred %>% select(text_id, description, donorname))
  }
  
  names(pred)[names(pred) == "predictions_raw"] <- gen_identifier
  pred_all <- pred_all %>%
    left_join(pred %>% select(-predictions, -text_cleaned, -donorname, -gender_filter, -description), by = "text_id")
  
}

#saveRDS(pred_all, "./Data/Gender permutations/pred_all_12345678.rds")

pred_all_tmp <- pred_all %>%
  mutate(description = NULL) %>%
  left_join(df_crs_original %>% 
              filter(!is.na(description_comb)) %>% 
              select(text_id, description = longdescription, text_detection = match_gender, 
                     gen_donor, gen_ppcode, gen_marker, gen_sdg, text_detection_stat = text_detection_wo_mining_w_scb), 
            by = "text_id") %>%
  select(text_id, description, donorname, match_gender, match_gender_gen_donor, match_gender_gen_marker,
         match_gender_gen_ppcode, match_gender_gen_sdg, match_gender_gen_donor_gen_ppcode, match_gender_gen_donor_gen_ppcode_gen_marker_gen_sdg,
         match_gender_intersection = 'match_gender_(gen_donor&gen_ppcode&gen_marker&gen_sdg)',
         text_detection, gen_donor, gen_marker, gen_ppcode, gen_sdg, text_detection_stat)

write.xlsx(pred_all_tmp, file = "./Tmp/XGBoost/Gender permutation results/pred_all.xlsx", rowNames = FALSE)

#pred_for_all <- read.xlsx("./Tmp/XGBoost/Gender permutation results/pred_all.xlsx", rowNames = FALSE)
#pred_for_all <- df_crs %>%
# filter(text_id %in% pred_for_all$text_id)

pred_xlsx <- read.xlsx("./Tmp/XGBoost/Gender permutation results/pred_all.xlsx", rowNames = FALSE)
pred_xlsx <- pred_xlsx %>%
  left_join(pred_all, by = c("text_id")) %>%
  select(text_id, description, match_gender, match_gender_gen_donor, match_gender_gen_marker,
         match_gender_gen_ppcode, match_gender_gen_sdg, match_gender_gen_donor_gen_ppcode, 
         match_gender_intersection = 'match_gender_(gen_donor&gen_ppcode&gen_marker&gen_sdg)',
         text_detection, gen_donor, gen_marker, gen_ppcode, gen_sdg, text_detection_stat)

write.xlsx(pred_xlsx, file = "./Tmp/XGBoost/Gender permutation results/pred_all_full.xlsx", rowNames = FALSE)
