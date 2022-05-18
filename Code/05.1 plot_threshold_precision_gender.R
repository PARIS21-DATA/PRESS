threshold_step <- data.frame(threshold = as.numeric(), accuracy = as.numeric(), precision = as.numeric(), recall = as.numeric())
intervall <- 0.01
for (i in seq(0, 1, by = intervall)) {
  # Crucial to decide the cut-off value or threshold - i.e., from what probability do we say an observation is gender_filter? 
  # The SDG lab uses a list of thresholds with a different threshold for each SDG. It remains unclear how they arrived at the threshold.
  # I will start with a simple 0.5. But this should be tested and optimized. 
  threshold <- i
  test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
  pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))
  
  # Check performance - confusion matrix - looks quite good!
  table(factor(test_data$predictions, levels=min(test_data$gender_filter):max(test_data$gender_filter)), 
        factor(test_data$gender_filter, levels=min(test_data$gender_filter):max(test_data$gender_filter)))
  
  # Prediction accuracy: 88 % 
  accuracy <- mean(test_data$predictions == test_data$gender_filter)
  true_pos <- test_data %>% filter(gender_filter == TRUE & predictions == 1) %>% nrow
  precision <- true_pos / (true_pos + test_data %>% filter(gender_filter == FALSE & predictions == 1) %>% nrow)
  recall <- true_pos / (true_pos + test_data %>% filter(gender_filter == TRUE & predictions == 0) %>% nrow)
  
  threshold_step <- threshold_step %>%
    rbind(c(i, accuracy, precision, recall))
}
names(threshold_step) <- c("threshold", "accuracy", "precision", "recall")

# Add F1 metric: F1 = 2*precision*recall/(precision + recall)
threshold_step <- threshold_step %>%
  mutate(F1 = 2*precision*recall/(precision + recall))
max_F1 <- threshold_step[threshold_step$F1 == max(threshold_step$F1, na.rm = T),]$threshold %>% na.omit

ggplot(threshold_step, aes(x = threshold)) +
  geom_line(aes(y = precision, colour = "precision")) + 
  geom_line(aes(y = accuracy, colour = "accuracy")) + 
  geom_line(aes(y = recall, colour = "recall")) +
  geom_line(aes(y = F1, colour = "F1")) +
  geom_vline(xintercept = max_F1, linetype = "dotted" ) +
  ylab("value") +
  ggtitle(paste0("Precision trajectory after ", it_add, " in intervalls ", intervall, " for a negative marked ration of ", neg_sample_fraction))
ggsave(paste0("./Tmp/XGBoost/Gender/threshold_precision_accuracy_", it_add, "_", neg_sample_fraction,"_n", nrow(df),"test+train.pdf"), width = 9, height = 7)

library(xlsx)
write.xlsx(test_data, file = "./Tmp/XGBoost/Gender/test_data.xlsx", row.names = FALSE)
write.xlsx(pred, file = "./Tmp/XGBoost/Gender/pred_data.xlsx", row.names = FALSE)
