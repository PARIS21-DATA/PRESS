
# Create manual verification sets to correct mistakes in learning set
positive_text_id <- df_crs %>% filter(stats_filter == TRUE) %>% pull(text_id) %>% unique

df_positive <- df_crs_original %>%
  filter(!duplicated(text_id)) %>%
  filter(text_id %in% positive_text_id & text_detection_wo_mining_w_scb == TRUE) %>% 
  select(text_id, longdescription) %>%
  mutate(Yes = rep("", n()), No = rep("", n()), NotSure = rep("", n()))

library(xlsx)
intervalls <- floor(seq(1, nrow(df_positive), 20))
for (i in 1:10) {
  write.xlsx(df_positive[intervalls[i]:(intervalls[i+1]-1),], file = paste0("./Tmp/XGBoost/Manual verification/train_data_", i,".xlsx"), row.names = FALSE)
}

write.xlsx(pred, file = "./Tmp/XGBoost/Manual verification/pred_data.xlsx", row.names = FALSE)


#----------------------- Read manual verification sets -------------------------

file_names_verified <- list.files(path = "./Data/Manually verified/")
file_names_verified <- paste0("./Data/Manually verified/", file_names_verified)

stat_projects_verified <- read.xlsx(file_names_verified[1], startRow = 1, cols = 1:5)

for (file in file_names_verified[2:length(file_names_verified)]) {
  file_current <- read.xlsx(file, startRow = 1, cols = 1:5)
  stat_projects_verified <- stat_projects_verified %>%
    rbind(file_current)
  
}

stat_projects_verified_tmp <- stat_projects_verified %>%
  mutate(match_stat = ifelse(Yes == "X" | Yes == "x" & !is.na(Yes), TRUE, FALSE)) %>%
  mutate(match_stat = replace_na(match_stat, FALSE)) %>%
  select(text_id, longdescription, match_stat)
