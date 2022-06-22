################################################################################
#
# XGBoost classification for gender projects
# Author: Guglielmo Zapalla, Johannes Abele, Yu Tian
# Date: Mai 2022
#
# Objective: Classify the projects with distinct long descriptions that were marked
#            as FALSE by the title detection into gender projects. The method
#            was adapted from the CSA classification but with additional options 
#            (iterative process, possibility of ngrams).
#
#
# 
# input files: - /Data/Intermediate/crs03_full.rds
#              - 
#              
#
# output file: - ./Tmp/XGBoost/importance_matrix_*.pdf
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------


# Clear environment
remove(list = ls())

setwd(getwd())

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
#lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)
rm(packages)

# Set paths
crs_path <- "./Data/intermediate/crs03_sample.rds"
crs_path_new <- "./Data/intermediate/positive_text_id.rds"

# Load data
df_crs <- readRDS(crs_path)
df_crs_original <- df_crs 
df_crs <- df_crs_original %>%
  filter(is.na(description_comb) == FALSE) %>%
  select(text_id, description = description_comb, gender_filter = match_gender, donorname) %>%
  distinct()

# Test duplicated long descriptions 
# freq_long <- as.data.frame(table(df_crs_original %>% pull(description_comb)))
# freq_long_gender <- as.data.frame(table(df_crs_original %>%
#                                    filter(match_gender == TRUE) %>% 
#                                    pull(description_comb)))
# freq_df <- as.data.frame(table(df %>%
#                       filter(gender_filter == TRUE) %>% 
#                       pull(description)))

#!!!WARNING: Whole process starts from here
for (iteration in c(FALSE, TRUE)) {

#-------------------------------- Set parameters -------------------------------

#iteration <- FALSE                # Set to FALSE to rerun the whole classification with adjusted learning set (negatively marked as ones with low probability in 0th iteration)
print_importance_matrix <- TRUE   # Set to TRUE to plot most important words
n_gram <- 1                       # Set to higher integers to use longer ngrams
neg_sample_fraction <- 1          # Fraction of negatively marked to positively marked in learning set
plot_results <- FALSE             # Set to TRUE to visualize results
frac_pred_set <- 0.05             # use only 5% of full prediction set to speed up for testing
save_fit_xgb <- TRUE              # Set to TRUE to save fitted xgb model


#---------------------- Define learning and prediction data --------------------

# We assign projects that were classified as statistical projects by title pattern matching to df
if (iteration) { 
  pred_negative <- pred %>% 
    filter(predictions_raw <= 0.3) %>%
    sample_n(size = neg_sample_fraction * df_crs %>% filter(gender_filter == TRUE) %>% nrow)  %>% 
    select(text_id, description, gender_filter, donorname) 
  
  df <- df_crs %>%
    filter(gender_filter == TRUE) %>%
    rbind(pred_negative) %>%
    filter(!is.na(gender_filter))
  
  pred <- df_crs %>%
    filter((gender_filter == FALSE | is.na(gender_filter)) & !(text_id %in% pred_negative$text_id)) %>%
    sample_n(size = floor(frac_pred_set * n())) #use only frac_pred_set% to speed up for testing
} else {
  pred <- df_crs %>%
    filter(gender_filter == FALSE | is.na(gender_filter)) %>%
    sample_n(size = floor(frac_pred_set * n())) #use only frac_pred_set% to speed up for testing
  
  df <- df_crs %>%
    filter(gender_filter == TRUE) %>%
    rbind(pred %>% filter(!is.na(gender_filter)) %>% sample_n(size = neg_sample_fraction * df_crs %>% filter(gender_filter == TRUE) %>% nrow))
}


#---------------------- Text cleaning and corpus creation ----------------------

# Define function to create corpus
create_corpus <- function (data){
  source <- VectorSource(data$description)
  corpus <- VCorpus(source) 
  corpus <- tm_map(corpus, content_transformer(tolower)) # lower case
  corpus <- tm_map(corpus, removeWords, c("'s")) # remove possesive s so that plural nouns get lemmatized correctly, e.g. "women's"
  corpus <- tm_map(corpus, removeNumbers)# remove numbers
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE) # remove punctuation. We want to keep dashes inside words such as in high-level
  corpus <- tm_map(corpus, stripWhitespace) # remove remaining white space
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'))) # remove stopwords for English
  corpus <- tm_map(corpus, removeWords, c(stopwords(source = "smart")[!stopwords(source = "smart") %in% "use"])) # remove some extra stopwords not captured by the previous list
  corpus <- tm_map(corpus, removeWords, c("iii")) # remove roman number 3 that is very common
  #corpus <- tm_map(corpus, replace_contraction) # extra step to catch exceptions such as "aren't" - might not be necessary
  corpus <- tm_map(corpus, lemmatize_strings) # lemmatize words - this might not be the best choice in particular with a English-French language mix
  corpus <- tm_map(corpus, PlainTextDocument) # transform into a format that can be used more easily later on
  return(corpus)
}

## Create corpus for df, pred 
corpus_df <- create_corpus(df)
corpus_df_pred <- create_corpus(pred)

# Integrate into the original dataframe that contains the domain labels
# convert corpus to dataframe
text_df <- t(data.frame(text = sapply(corpus_df, as.character), stringsAsFactors = FALSE))
rownames(text_df) <- NULL

text_df_pred <- t(data.frame(text = sapply(corpus_df_pred, as.character), stringsAsFactors = FALSE))
rownames(text_df_pred) <- NULL

# change original description with cleaned description
df$text_cleaned <- text_df[,1]
pred$text_cleaned <- text_df_pred[,1]


#---------------------- Training the Model: XGBoost ----------------------------

# Splitting in training and test 
dt <- sort(sample(nrow(df), nrow(df)*0.8))
train_data <- df[dt,]
test_data <- df[-dt,]

# Try ngram 
if (n_gram == 1) {
  control_list_ngram <- list(weighting = weightTf)
} else {
  NLP_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 1:n_gram), paste, collapse = " "), use.names = FALSE)
  }
  control_list_ngram = list(tokenize = NLP_tokenizer,
                            removePunctuation = TRUE,
                            removeNumbers = TRUE, 
                            stopwords = stopwords("english"), 
                            tolower = T, 
                            lemmatization = T, 
                            weighting = function(x) { weightTf(x) })
}
# Creating document-feature-matrix for training data and for total data
total_data_dtm <- df$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)
train_data_dtm <- train_data$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)

# Creating document-feature-matrix for test data. Here we have to keep two things in mind:
# 1. We have to exclude words that do not appear in the training data. The model does not know these and breaks.
# 2. We have to include words (although with a 0) that appear in the training data but not in the test data. Otherwise the model gets confused as well.
# See here for a detailed discussion: https://stackoverflow.com/questions/16630627/how-to-recreate-same-documenttermmatrix-with-new-test-data 
if (n_gram == 1) {
  control_list_ngram <- list(weighting = weightTf, dictionary=Terms(train_data_dtm))
} else {
  control_list_ngram = list(tokenize = NLP_tokenizer,
                            removePunctuation = TRUE,
                            removeNumbers = TRUE, 
                            stopwords = stopwords("english"), 
                            tolower = T, 
                            lemmatization = T, 
                            weighting = function(x) { weightTf(x) },
                            dictionary=Terms(train_data_dtm))
}
test_data_dtm <- test_data$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)
prediction_data_dtm <- pred$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)

# Train the model for one statistical domain - parameters taken from SDG lab code. These might benefit from tuning
eta_par <- 0.1
nrounds_par <- 5 / eta_par

# set the labels for gender_filter
label.train <- as.numeric(train_data$gender_filter)
label.test <- as.numeric(test_data$gender_filter)
label.prediction <- as.numeric(pred$gender_filter)

# Training the model
fit.xgb <- xgboost(data = as.matrix(train_data_dtm), label = label.train, max.depth = 17, eta = eta_par, nthread = 2, 
                   nrounds = nrounds_par, objective = "binary:logistic", verbose = 1)

# Save the fitted model so that it can be loaded later on (especially for very large training sets)
if (save_fit_xgb) save(fit.xgb, file = "./Tmp/XGBoost/Fitted models/fit.xgb.gender.Rdata")

# Uncomment to load previously fitted model
#fit.xgb <- load("./Tmp/XGBoost/Fitted models/fit.xgb.gender.Rdata")

# Check which words have a high importance for the prediction
if (print_importance_matrix) {
  importance.matrix <- xgb.importance(model = fit.xgb)
  it_add <- "it0"
  if (iteration) it_add <- "it1"
  pdf(paste0("./Tmp/XGBoost/Gender/importance_matrix_", it_add,"_", neg_sample_fraction, "_n", nrow(df), "test+train.pdf"))
  xgb.plot.importance(importance.matrix, top_n = 30, rel_to_first = TRUE, xlab = "Relative importance")
  dev.off()
}

# Predict gender_filter based on the text in the test set and in the prediction set
pred.xgb <- predict(fit.xgb, as.matrix(test_data_dtm))
pred1.xgb <- predict(fit.xgb, as.matrix(prediction_data_dtm))

# add predictions to test and prediction data
test_data$predictions_raw <- pred.xgb
pred$predictions_raw <- pred1.xgb

# Crucial to decide the cut-off value or threshold - i.e., from what probability do we say an observation is gender_filter? 
# The SDG lab uses a list of thresholds with a different threshold for each SDG. It remains unclear how they arrived at the threshold.
threshold <- 0.95
test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))

# Check performance - confusion matrix - looks quite good!
table(factor(test_data$predictions, levels=min(test_data$gender_filter):max(test_data$gender_filter)), 
      factor(test_data$gender_filter, levels=min(test_data$gender_filter):max(test_data$gender_filter)))

# Prediction accuracy:  % 
precision <- test_data %>% filter(predictions == 1 & gender_filter == TRUE) %>% nrow
precision <- precision / (precision + test_data %>% filter(predictions == 1 & gender_filter == FALSE) %>% nrow)
print(paste0("Accuracy: ", mean(test_data$predictions == test_data$gender_filter)))
print(paste0("Precision: ", precision))
print(paste0("Fraction of detected projects: ", mean(pred$predictions)))

}

#---------------------------- Visualization  -----------------------------------

# Plot accuracy and precision trajectories
if (plot_results) source("./Code/05.1 visualize_XGBoost_results_gender.R")



