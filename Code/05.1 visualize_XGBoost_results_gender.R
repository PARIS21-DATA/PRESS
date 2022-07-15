################################################################################
#
# Visualization of XGBoost results 
# Author: Johannes Abele
# Date: May 2022
#
# Objective: Visualize the results from the gender classification. 
#            1. Plot of the precision, accuracy and recall for different thresholds
#            2. Histogram of amount of words-distribution with color acc. to classification
#            3. Histogram of donor distribution for learning set ""
#            4. Histogram of donor distribution for prediction set ""
#            5. Histogram of the sectors for learning set ""
#            
# 
# input files: - uses data frames from previous gender classification
#              
#
# output file: - 1. - 5. 
#
#
################################################################################


# --------------------- Plot precision, accuracy and recall --------------------

threshold_step <- data.frame(threshold = as.numeric(), accuracy = as.numeric(), precision = as.numeric(), recall = as.numeric())
intervall <- 0.01
for (i in seq(0, 1, by = intervall)) {
  
  threshold <- i
  test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
  pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))
  
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
ggsave(paste0("./Tmp/XGBoost/Gender/", lang , "_threshold_precision_accuracy_", it_add, "_", neg_sample_fraction,"_n", nrow(df),"learning.pdf"), width = 9, height = 7)

#write.xlsx(test_data, file = "./Tmp/XGBoost/Gender/test_data.xlsx", row.names = FALSE)
#write.xlsx(pred, file = "./Tmp/XGBoost/Gender/pred_data.xlsx", row.names = FALSE)


#----------------------------- Plot histograms ---------------------------------

threshold <- 0.95
pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))

pred <- pred %>% 
  mutate(total = str_count(string = text_cleaned, pattern = "\\S+")) %>%
  mutate(predictions = as.factor(predictions))

# Histograms of word distributions  
hist_word_count_distr <- ggplot(pred, aes(x = total, fill = predictions)) + 
  geom_histogram(binwidth = 2) + 
  xlab("Number of words in description combination") +
  ylab("Number of documents") + 
  ggtitle(paste0("Word distribution with binwidth 2 for threshold of ", threshold))
ggsave(paste0("./Tmp/XGBoost/Gender/", lang , "_word_distr_", it_add, "_", neg_sample_fraction,"_n", nrow(df),"_learning.pdf"), width = 9, height = 7)

# Histogram of donor/sector frequency
# df <- df %>%
#   left_join(df_crs %>% select(donorname, text_id), by = "text_id") %>% 
#   mutate(donorname = as.factor(donorname))

# Test data
ggplot(df, aes(x = donorname, fill = gender_filter)) + 
  geom_bar() + 
  xlab("Donorname") +
  ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle(paste0("Donor distribution in train + test data (n=", nrow(df), ")"))
ggsave(paste0("./Tmp/XGBoost/Gender/", lang , "_hist_donorname_learning_data.pdf"), width = 13, height = 7)

# Pred data
ggplot(pred, aes(x = donorname, fill = predictions)) + 
  geom_bar() + 
  xlab("Donorname") +
  ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle(paste0("Donor distribution in pred data (pred data n=", nrow(pred), ")"))
ggsave(paste("./Tmp/XGBoost/Gender/", lang , "_hist_donorname_pred_data.pdf"), width = 15, height = 9)

# Sector test data
# ggplot(df, aes(x = sectorname, fill = gender_filter)) + 
#   geom_bar() + 
#   xlab("Sector") +
#   ylab("Count") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   ggtitle(paste0("Sector distribution in test + test data (n=", nrow(df), ")"))
# ggsave("./Tmp/XGBoost/Gender/", lang , "_hist_sector_learning_data.pdf", width = 11, height = 7)
