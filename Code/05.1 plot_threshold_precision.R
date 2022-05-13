threshold_step <- data.frame(threshold = as.numeric(), accuracy = as.numeric(), precision = as.numeric())
intervall <- 0.01
for (i in seq(0, 1, by = intervall)) {
  # Crucial to decide the cut-off value or threshold - i.e., from what probability do we say an observation is stats_filter? 
  # The SDG lab uses a list of thresholds with a different threshold for each SDG. It remains unclear how they arrived at the threshold.
  # I will start with a simple 0.5. But this should be tested and optimized. 
  threshold <- i
  test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
  pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))
  
  # Check performance - confusion matrix - looks quite good!
  table(factor(test_data$predictions, levels=min(test_data$stats_filter):max(test_data$stats_filter)), 
        factor(test_data$stats_filter, levels=min(test_data$stats_filter):max(test_data$stats_filter)))
  
  # Prediction accuracy: 88 % 
  accuracy <- mean(test_data$predictions == test_data$stats_filter)
  precision <- test_data %>% filter(predictions == 1 & stats_filter == TRUE) %>% nrow
  precision <- precision / (precision + test_data %>% filter(predictions == 1 & stats_filter == FALSE) %>% nrow)
  
  threshold_step <- threshold_step %>%
    rbind(c(i, accuracy, precision))
}
names(threshold_step) <- c("threshold", "accuracy", "precision")

ggplot(threshold_step, aes(x = threshold)) +
  geom_line(aes(y = precision, colour = "precision")) + 
  geom_line(aes(y = accuracy, colour = "accuracy")) + 
  ylab("value")+
  #geom_point() +
  ggtitle(paste0("Precision trajectory after it 2 in intervalls ", intervall))
ggsave(paste0("./Tmp/XGBoost/threshold_precision_accuracy_n", nrow(df_crs),"sample.pdf"), width = 9, height = 7)

library(xlsx)
write.xlsx(test_data, file = "./Tmp/XGBoost/test_data.xlsx", row.names = FALSE)
write.xlsx(pred, file = "./Tmp/XGBoost/pred_data.xlsx", row.names = FALSE)
