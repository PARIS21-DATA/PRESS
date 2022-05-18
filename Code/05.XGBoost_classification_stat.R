#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# 2021-07-21
# Script classify PRESS projects into CSA domains from 2012 onwards
# Guglielmo Zappala

# Input files: - projects_to_classify_for_csa.csv
#              - new_PRESS.csv

# Output files: - classified_PRESS_2012.csv
#               - classified_crs_csa.csv

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Objective
# The purpose of this script is to take the PRESS 2020 data, clean the project title and description
# and a build an algorithms using XGBoost classifier to predict CSA domains from descriptions
# dropping all years before 2012
#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%

# Clear environment
remove(list = ls())

# Load required libraries
packages <-
  c(
    "tidyverse",
    "e1071",
    "caTools",
    "caret",
    "tm",
    "textstem",
    "tidytext",
    "mgsub",
    "textclean",
    "lexicon",
    "wordcloud",
    "quanteda",
    "textcat",
    "text2vec",
    "xgboost",
    "rlist",
    "remotes",
    "ParBayesianOptimization",
    "mlr",
    "DiagrammeR",
    "cld2"
  )
# Install uninstalled packages
lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)


#%#%#%#%#%#%#%#%#%#%
# Loading data
#%#%#%#%#%#%#%#%#%#%

# setting working directory for experimenting - in the future loading data from github might be the easier solution
setwd(getwd())

crs_path <- "./Data/intermediate/crs03.rds"
crs_path_new <- "./Data/intermediate/positive_text_id.rds"
lang <-  "en"
language <- "english"
df_crs <- readRDS(crs_path)
df_crs_original <- df_crs 
df_crs <- df_crs_original %>%
  filter(is.na(description_comb) == FALSE) %>%
  select(text_id, description = description_comb, stats_filter = text_detection_wo_mining_w_scb) %>%
  distinct()

library(xlsx)
write.xlsx(df_crs_original, file = "./Tmp/XGBoost/full_unclassified_crs.xlsx", row.names = FALSE)

# Test duplicated long descriptions 
freq_long <- as.data.frame(table(df_crs_original %>% pull(description_comb)))
freq_long_stat <- as.data.frame(table(df_crs_original %>%
                                   filter(text_detection_wo_mining_w_scb == TRUE) %>% 
                                   pull(description_comb)))
freq_df <- as.data.frame(table(df %>%
                      filter(stats_filter == TRUE) %>% 
                      pull(description)))

#%#%#%#%#%#%#%#%#%#%
# Preparing the target variable Y: Statistical Activity
#%#%#%#%#%#%#%#%#%#%


# We assign projects that were not classified as statistical projects by title pattern matching to the prediction data frame pred

# We assign projects that were classified as statistical projects by title pattern matching to df
iteration <- TRUE
print_importance_matrix <- TRUE
neg_sample_fraction <- 1
if (iteration) { 
  pred_negative <- pred %>% 
    filter(predictions_raw <= 0.3) %>%
    sample_n(size = neg_sample_fraction * df_crs %>% filter(stats_filter == TRUE) %>% nrow)  %>% 
    select(text_id, description, stats_filter) 
  
  df <- df_crs %>%
    filter(stats_filter == TRUE) %>%
    rbind(pred_negative) %>%
    filter(!is.na(stats_filter))
  
  pred <- df_crs %>%
    filter((stats_filter == FALSE | is.na(stats_filter)) & !(text_id %in% pred_negative$text_id)) %>%
    sample_n(size = floor(0.05 * n())) #use only 5% to speed up for testing
} else {
  pred <- df_crs %>%
    filter(stats_filter == FALSE | is.na(stats_filter)) %>%
    sample_n(size = floor(0.05 * n())) #use only 5% to speed up for testing
  
  df <- df_crs %>%
    filter(stats_filter == TRUE) %>%
    rbind(pred %>% sample_n(size = neg_sample_fraction * df_crs %>% filter(stats_filter == TRUE) %>% nrow)) %>%
    filter(!is.na(stats_filter))
}
 


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#### Text cleaning and corpus creation #
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# IMPORTANT: 1. Stemming vs. lemmatization vs. original words

# Define function to create corpus
create_corpus <- function (data){
  source <- VectorSource(data$description)
  corpus <- VCorpus(source) 
  corpus <- tm_map(corpus, content_transformer(tolower)) # lower case
  corpus <- tm_map(corpus, removeNumbers)# remove numbers
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE) # remove punctuation. We want to keep dashes inside words such as in high-level
  corpus <- tm_map(corpus, stripWhitespace) # remove remaining white space
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'))) # remove stopwords for English
  corpus <- tm_map(corpus, removeWords, c(stopwords(source = "smart"))) # remove some extra stopwords not captured by the previous list
  corpus <- tm_map(corpus, removeWords, c("iii")) # remove roman number 3 that is very common
  corpus <- tm_map(corpus, removeWords, c(stopwords("fr"))) # remove french stopwords
  #corpus <- tm_map(corpus, replace_contraction) # extra step to catch exceptions such as "aren't" - might not be necessary
  corpus <- tm_map(corpus, lemmatize_strings) # lemmatize words - this might not be the best choice in particular with a English-French language mix
  corpus <- tm_map(corpus, PlainTextDocument) # transform into a format that can be used more easily later on
  return(corpus)
}

# If we can it would be great to remove roman numbers but we might be able to catch them by removing very rare words later on
## Create corpus for df, pred 
corpus_df <- create_corpus(df)
corpus_df_pred <- create_corpus(pred)


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#### Data Exploration/Testing ###
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#

######## Integrate into the original dataframe that contains the domain labels

# convert corpus to dataframe
text_df <- t(data.frame(text = sapply(corpus_df, as.character), stringsAsFactors = FALSE))
rownames(text_df) <- NULL

text_df_pred <- t(data.frame(text = sapply(corpus_df_pred, as.character), stringsAsFactors = FALSE))
rownames(text_df_pred) <- NULL

# change original description with cleaned description
df$text_cleaned <- text_df[,1]
pred$text_cleaned <- text_df_pred[,1]

#%#%#%#%#%#%#%#%#%#%%#%#%%#%#
# Training the Model: XGBoost
#%#%#%#%#%#%#%#%#%#%%#%#%%#%#

###########################################
### Test run for ONE category: domain 1 ###
###########################################

# Splitting in training and test - here you use as training all 
dt <- sort(sample(nrow(df), nrow(df)*0.8))
train_data <- df[dt,]
test_data <- df[-dt,]

# Try ngram 
NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
}
control_list_ngram = list(tokenize = NLP_tokenizer,
                          removePunctuation = TRUE,
                          removeNumbers = TRUE, 
                          stopwords = stopwords("english"), 
                          tolower = T, 
                          lemmatization = T, 
                          weighting = function(x) { weightTf(x) },
                          dictionary=Terms(train_data_dtm) # only for test and prediction data 
                          )

control_list_ngram <- list(weighting = weightTf)

# Creating document-feature-matrix for training data and for total data
total_data_dtm <- df$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)
train_data_dtm <- train_data$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)

# Creating document-feature-matrix for test data. Here we have to keep two things in mind:
# 1. We have to exclude words that do not appear in the training data. The model does not know these and breaks.
# 2. We have to include words (although with a 0) that appear in the training data but not in the test data. Otherwise the model gets confused as well.
# See here for a detailed discussion: https://stackoverflow.com/questions/16630627/how-to-recreate-same-documenttermmatrix-with-new-test-data 
control_list_ngram_dict <- list(weighting = weightTf, dictionary=Terms(train_data_dtm))
test_data_dtm <- test_data$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram_dict)
prediction_data_dtm <- pred$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram_dict)

# Train the model for one statistical domain - parameters taken from SDG lab code. These might benefit from tuning
eta_par <- 0.1
nrounds_par <- 5 / eta_par

# set the labels for stats_filter
label.train <- as.numeric(train_data$stats_filter)
label.test <- as.numeric(test_data$stats_filter)
label.prediction <- as.numeric(pred$stats_filter)

# Training the model
fit.xgb <- xgboost(data = as.matrix(train_data_dtm), label = label.train, max.depth = 17, eta = eta_par, nthread = 2, 
                   nrounds = nrounds_par, objective = "binary:logistic", verbose = 1)

# Change importance matrix
#xgb.importance(model = fit.xgb, )

# Check which words have a high importance for the prediction
if (print_importance_matrix) {
  importance.matrix <- xgb.importance(model = fit.xgb)
  it_add <- "it0"
  if (iteration) it_add <- "it1"
  pdf(paste0("./Tmp/XGBoost/importance_matrix_", it_add,"_", neg_sample_fraction, "_n", nrow(df), "test+train.pdf"))
  xgb.plot.importance(importance.matrix, top_n = 30, rel_to_first = TRUE, xlab = "Relative importance")
  dev.off()
}

# Predict stats_filter based on the text in the test set and in the prediction set
pred.xgb <- predict(fit.xgb, as.matrix(test_data_dtm))
pred1.xgb <- predict(fit.xgb, as.matrix(prediction_data_dtm))

# add predictions to test and prediction data
test_data$predictions_raw <- pred.xgb
pred$predictions_raw <- pred1.xgb

# Crucial to decide the cut-off value or threshold - i.e., from what probability do we say an observation is stats_filter? 
# The SDG lab uses a list of thresholds with a different threshold for each SDG. It remains unclear how they arrived at the threshold.
# I will start with a simple 0.5. But this should be tested and optimized. 
threshold <- 0.95
test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))

# Check performance - confusion matrix - looks quite good!
table(factor(test_data$predictions, levels=min(test_data$stats_filter):max(test_data$stats_filter)), 
      factor(test_data$stats_filter, levels=min(test_data$stats_filter):max(test_data$stats_filter)))

# Prediction accuracy: 88 % 
precision <- test_data %>% filter(predictions == 1 & stats_filter == TRUE) %>% nrow
precision <- precision / (precision + test_data %>% filter(predictions == 1 & stats_filter == FALSE) %>% nrow)
print(paste0("Accuracy: ", mean(test_data$predictions == test_data$stats_filter)))
print(paste0("Precision: ", precision))
print(paste0("Fraction of detected projects: ", mean(pred$predictions)))



#---------------------------- Visualization  -----------------------------------------

# Plot accuracy and precision trajectories
source("./Code/05.1 plot_threshold_precision.R")

pred <- pred %>% mutate(total = str_count(string = text_cleaned, pattern = "\\S+")) %>%
  mutate(predictions = as.factor(predictions))

# Histograms of word distributions  
hist_word_count_distr <- ggplot(pred, aes(x = total, fill = predictions)) + 
  geom_histogram(binwidth = 2) + 
  xlab("Number of words in description combination") +
  ylab("Number of documents") + 
  ggtitle(paste0("Word distribution with binwidth 2 for threshold of ", threshold))
ggsave(paste0("./Tmp/XGBoost/word_distr_", it_add, "_", neg_sample_fraction,"_n", nrow(df),"test+train.pdf"), width = 9, height = 7)

# Histogram of donor/sector frequency
df <- df %>%
  left_join(df_crs %>% select(donorname, sectorname, text_id), by = "text_id") %>% 
  mutate(donorname = as.factor(donorname),
         sectorname = as.factor(sectorname))

# Test data
ggplot(df, aes(x = donorname, fill = stats_filter)) + 
  geom_bar() + 
  xlab("Donorname") +
  ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle(paste0("Donor distribution in train + test data (n=", nrow(df), ")"))
ggsave("./Tmp/XGBoost/hist_donorname_train+test_data.pdf", width = 11, height = 7)

# Pred data
ggplot(pred, aes(x = donorname)) + 
  geom_bar() + 
  xlab("Donorname") +
  ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle(paste0("Donor distribution in test data (pred data n=", nrow(pred), ")"))
ggsave("./Tmp/XGBoost/hist_donorname_pred_data.pdf", width = 11, height = 7)

# Sector test data
ggplot(df, aes(x = sectorname, fill = stats_filter)) + 
  geom_bar() + 
  xlab("Sector") +
  ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle(paste0("Sector distribution in test + test data (n=", nrow(df), ")"))
ggsave("./Tmp/XGBoost/hist_sector_train+test_data.pdf", width = 11, height = 7)
  

