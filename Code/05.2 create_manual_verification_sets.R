
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

files <- read.xlsx(file_names_verified[1], startRow = 1, cols = 1:5)
