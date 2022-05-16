
positive_text_id <- df_crs %>% filter(stats_filter == TRUE) %>% pull(text_id) %>% unique

df_positive <- df_crs_original %>%
  filter(text_id %in% positive_text_id) %>% 
  filter(!duplicated(text_id)) %>%
  select(text_id, longdescription)

intervalls <- floor(seq(1, nrow(df_positive), nrow(df_positive)/20))
for (i in 1:19) {
  write.xlsx(df_positive[intervalls[i]:intervalls[i+1],], file = paste0("./Tmp/XGBoost/Manual verification/train_data_", i,".xlsx"), row.names = FALSE)
}

library(xlsx)
write.xlsx(pred, file = "./Tmp/XGBoost/Manual verification/pred_data.xlsx", row.names = FALSE)

 