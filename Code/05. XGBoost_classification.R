################################################################################
#
# XGBoost classification for gender or statistical projects
# Author: Guglielmo Zapalla, Johannes Abele, Yu Tian
# Date: Mai 2022
#
# Objective: Classify the projects with distinct long descriptions that were marked
#            as FALSE by the title detection into gender/stat projects. The method
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
source("./Code/00. boot.R")
source("./Code/00.1 text_preparation_functions.R")

# Set paths
crs_path <- "./Data/Intermediate/crs03_full.rds"
#crs_path_new <- "./Data/Intermediate/positive_text_id.rds"

# Load data
df_crs <- readRDS(crs_path)
# df_crs_raw <- df_crs
# df_crs <- df_crs_raw

# Set specific language
lang <- "en"
df_crs <- df_crs %>%
  filter(title_language %in% c(lang, NA) & long_language %in% c(lang,NA)) %>%
  filter(!is.na(title_language) | !is.na(long_language))

# Set languages for stemming and lemmatization
stem_languages <- c("de", "fr", "es")
lemma_languages <- c("en")


################################################################################
# Set filter for classification: 
# use "gender" for gender classification, "stat" for statistical classification

class_type <- "gender"
################################################################################



# Reduce data set to variables important for prediction
df_crs_original <- df_crs 
if (class_type == "gender"){
  df_crs <- df_crs_original %>%
    filter(!is.na(descr2mine)) %>%
    select(text_id, description = descr2mine, class_filter = match_gender) %>%
    distinct() %>%
    group_by(text_id) %>% # remove all ambiguous projects (same description, one FALSE one TRUE)
    filter(n() == 1) %>%
    ungroup() %>%
    as.data.frame
} else if (class_type == "stat"){
  man_verified <- readRDS("./Data/Manually verified/stat_projects_verified.rds")
  df_crs <- df_crs_original %>%
    filter(!is.na(descr2mine)) %>%
    select(text_id, description = descr2mine, longdescription, class_filter = text_detection_wo_mining_w_scb) %>%
    left_join(man_verified %>% select(longdescription, match_stat), by = "longdescription") %>%
    mutate(class_filter = ifelse(!is.na(match_stat), match_stat, class_filter)) %>% # integrate manually verified
    select(-longdescription, -match_stat) %>%
    distinct() %>%
    group_by(text_id) %>% # remove all ambiguous projects (same description, one FALSE one TRUE)
    filter(n() == 1) %>%
    ungroup() %>%
    as.data.frame
}
rm(df_crs_original, man_verified)  

# Test duplicated long descriptions 
# freq_long <- as.data.frame(table(df_crs_original %>% pull(descr2mine)))
# freq_long_gender <- as.data.frame(table(df_crs_original %>%
#                                    filter(match_gender == TRUE) %>% 
#                                    pull(descr2mine)))
# freq_df <- as.data.frame(table(df %>%
#                       filter(class_filter == TRUE) %>% 
#                       pull(description)))

#-------------------------------- Set parameters -------------------------------

#iteration <- TRUE              # Set to FALSE to rerun the whole classification with adjusted learning set (negatively marked as ones with low probability in 0th iteration)
print_importance_matrix <- TRUE   # Set to TRUE to plot most important words
n_gram <- 1                       # Set to higher integers to use longer ngrams
full_learning_percent <- 0.4       # take only x% of full learning set size is too large for RAM
neg_sample_fraction <- 1          # Fraction of negatively marked to positively marked in learning set
plot_results <- TRUE             # Set to TRUE to visualize results
frac_pred_set <- 1             # use only 5% of full prediction set to speed up for testing
save_fit_xgb <- TRUE              # Set to TRUE to save fitted xgb model
load_fit_xgb <- FALSE             # load previously fitted model
split_pred <- TRUE                # use to split up pred data into two data frames to handle large pred sets
n_pred_sets <- 30                 # number of splitted data prediction sets


#!!!WARNING: Whole process starts from here
start <- Sys.time()
for (iteration in c(FALSE, TRUE)) {

#---------------------- Define learning and prediction data --------------------

size_positive_train <- neg_sample_fraction * full_learning_percent * df_crs %>% filter(class_filter == TRUE) %>% nrow

# We assign projects that were classified as statistical projects by title pattern matching to df
if (iteration) { 
  pred_negative <- pred %>% 
    filter(predictions_raw <= 0.3) %>%
    sample_n(size = size_positive_train) %>% 
    select(text_id, description, class_filter) 
  
  df <- df_crs %>%
    filter(class_filter == TRUE) %>%
    sample_n(size = n()*full_learning_percent) %>%
    rbind(pred_negative) %>%
    filter(!is.na(class_filter))
  
  pred <- df_crs %>%
    filter((class_filter == FALSE | is.na(class_filter)) & !(text_id %in% pred_negative$text_id)) %>%
    sample_n(size = frac_pred_set * n()) #use only frac_pred_set% to speed up for testing
} else {
  pred <- df_crs %>%
    filter(class_filter == FALSE | is.na(class_filter)) %>%
    sample_n(size = frac_pred_set * n()) #use only frac_pred_set% to speed up for testing
  
  if(pred %>% filter(!is.na(class_filter)) %>% nrow < size_positive_train) stop("Pred not large enough to create learning set! Choose a larger frac_pred_set")
  
  df <- df_crs %>%
    filter(class_filter == TRUE) %>%
    sample_n(size = n()*full_learning_percent) %>%
    rbind(pred %>% filter(!is.na(class_filter)) %>% sample_n(size = size_positive_train))
  
  pred <- pred %>%
    filter(!text_id %in% df$text_id)
}

print(paste0("Size of learning data set: ", nrow(df)))
print(paste0("Size of prediction data set: ", nrow(pred)))

#---------------------- Text cleaning and corpus creation ----------------------

# # Define function to create corpus
# create_corpus <- function (data, language = "en"){
#   source <- VectorSource(data$description)
#   corpus <- VCorpus(source) 
#   corpus <- tm_map(corpus, content_transformer(tolower)) # lower case
#   corpus <- tm_map(corpus, removeWords, c("'s")) # remove possesive s so that plural nouns get lemmatized correctly, e.g. "women's"
#   corpus <- tm_map(corpus, removeNumbers)# remove numbers
#   corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE) # remove punctuation. We want to keep dashes inside words such as in high-level
#   corpus <- tm_map(corpus, stripWhitespace) # remove remaining white space
#   corpus <- tm_map(corpus, removeWords, c(stopwords(language = langauge))) # remove stopwords for English
#   corpus <- tm_map(corpus, removeWords, c(stopwords(source = "smart")[!stopwords(source = "smart") %in% "use"])) # remove some extra stopwords not captured by the previous list
#   corpus <- tm_map(corpus, removeWords, c("iii")) # remove roman number 3 that is very common
#   #corpus <- tm_map(corpus, replace_contraction) # extra step to catch exceptions such as "aren't" - might not be necessary
#   corpus <- tm_map(corpus, lemmatize_strings) # lemmatize words - this might not be the best choice in particular with a English-French language mix
#   corpus <- tm_map(corpus, PlainTextDocument) # transform into a format that can be used more easily later on
#   return(corpus)
# }
# 
# ## Create corpus for df, pred 
# corpus_df <- create_corpus(df)
# corpus_df_pred <- create_corpus(pred)
# 
# # Integrate into the original dataframe that contains the domain labels
# # convert corpus to dataframe
# text_df <- t(data.frame(text = sapply(corpus_df, as.character), stringsAsFactors = FALSE))
# rownames(text_df) <- NULL
# 
# text_df_pred <- t(data.frame(text = sapply(corpus_df_pred, as.character), stringsAsFactors = FALSE))
# rownames(text_df_pred) <- NULL
# 
# # change original description with cleaned description
# df$text_cleaned <- text_df[,1]
# pred$text_cleaned <- text_df_pred[,1]

# change original description with cleaned description
if (lang %in% lemma_languages) {
  df$text_cleaned <- clean_and_lemmatize(df$description, language = lang)
  print("Start lemmatize pred")
  pred$text_cleaned <- clean_and_lemmatize(pred$description, language = lang)
  print("Finished lemmatization pred")
} else if (lang %in% stem_languages) {
  df$text_cleaned <- stem_and_concatenate(df$description, language = lang)
  pred$text_cleaned <- stem_and_concatenate(pred$description, language = lang)
}


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
                            stopwords = stopwords(lang), 
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
                            stopwords = stopwords(lang), 
                            tolower = T, 
                            lemmatization = T, 
                            weighting = function(x) { weightTf(x) },
                            dictionary=Terms(train_data_dtm))
}
test_data_dtm <- test_data$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)

print("Start to create pred DTM")
if (split_pred) {
  pred_splitted <- suppressWarnings(split(pred, (0:nrow(pred) %/% ceiling(nrow(pred)/(n_pred_sets)))))
  pred_splitted <- lapply(pred_splitted, function(y) {y %>% pull(text_cleaned) %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)})
  print("Pred DTM finished")
} else {
  prediction_data_dtm <- pred$text_cleaned %>% VectorSource() %>% VCorpus() %>% DocumentTermMatrix(control = control_list_ngram)
  print("Pred DTM finished")
}

# Train the model for one statistical domain - parameters taken from SDG lab code. These might benefit from tuning
eta_par <- 0.1
nrounds_par <- 5 / eta_par

# set the labels for class_filter
label.train <- as.numeric(train_data$class_filter)
label.test <- as.numeric(test_data$class_filter)

# Training the model
# Load previously fitted model, if not available fit 
xgb.filename <- paste0("./Tmp/XGBoost/Fitted models/fit.xgb.", class_type, "_", lang)
if (!load_fit_xgb) {
  fit.xgb <- xgboost(data = as.matrix(train_data_dtm), label = label.train, max.depth = 17, eta = eta_par, nthread = 2, 
                     nrounds = nrounds_par, objective = "binary:logistic", verbose = 1)
} else if (file.exists(xgb.filename) & load_fit_xgb) {
  fit.xgb <- readRDS(xgb.filename)
} else if (!file.exists(xgb.filename) & load_fit_xgb) {
  stop(paste0("No model found: ", "./Tmp/XGBoost/Fitted models/fit.xgb.", class_type, "_", lang))
}
  
if (!file.exists(xgb.filename) & iteration & save_fit_xgb){
  saveRDS(fit.xgb, xgb.filename)
}



# Check which words have a high importance for the prediction
it_add <- "it0"
if (iteration) it_add <- "it1"
if (print_importance_matrix) {
  importance.matrix <- xgb.importance(model = fit.xgb)
  pdf(paste0("./Tmp/XGBoost/", class_type, "/", lang , "_importance_matrix_", it_add,"_", neg_sample_fraction, "_n", nrow(df), "learning.pdf"))
  xgb.plot.importance(importance.matrix, top_n = 30, rel_to_first = TRUE, xlab = "Relative importance")
  dev.off()
}

# Predict class_filter based on the text in the test set and in the prediction set
#shared_features <- colnames(as.matrix(test_data_dtm)) %in% fit.xgb$feature_names
test.xgb <- predict(fit.xgb, as.matrix(test_data_dtm))
if (split_pred) {
  pred_splitted <- map(pred_splitted, ~ predict(fit.xgb, as.matrix(.x)))
} else {
  pred.xgb <- predict(fit.xgb, as.matrix(prediction_data_dtm))
}


# add predictions to test and prediction data
test_data$predictions_raw <- test.xgb
if (split_pred) {
  pred_splitted <- unlist(pred_splitted)
  pred$predictions_raw <- pred_splitted
} else {
  pred$predictions_raw <- pred.xgb
}

# Crucial to decide the cut-off value or threshold - i.e., from what probability do we say an observation is class_filter? 
# The SDG lab uses a list of thresholds with a different threshold for each SDG. It remains unclear how they arrived at the threshold.
threshold <- 0.95
test_data <- mutate(test_data, predictions = ifelse(predictions_raw > threshold, 1, 0))
pred <- mutate(pred, predictions = ifelse(predictions_raw > threshold, 1, 0))
saveRDS(pred, paste0("./Tmp/XGBoost/", class_type, "/prediction sets/", lang, "_pred_", it_add, ".rds"))

# Check performance - confusion matrix - looks quite good!
table(factor(test_data$predictions, levels=min(test_data$class_filter):max(test_data$class_filter)), 
      factor(test_data$class_filter, levels=min(test_data$class_filter):max(test_data$class_filter)))

# Prediction accuracy:  % 
precision <- test_data %>% filter(predictions == 1 & class_filter == TRUE) %>% nrow
precision <- precision / (precision + test_data %>% filter(predictions == 1 & class_filter == FALSE) %>% nrow)
print(paste0("Accuracy: ", mean(test_data$predictions == test_data$class_filter)))
print(paste0("Precision: ", precision))
print(paste0("Fraction of detected projects: ", mean(pred$predictions)))

}
difftime(Sys.time(),start, units = "sec")

#---------------- Merge prediction results with original data ------------------

# To load classification results
#pred <- readRDS(paste0("./Tmp/XGBoost/", class_type, "/prediction sets/", lang, "_pred_", it_add, ".rds"))

# To join stat classification to final gender crs data set
# df_crs_final <- df_crs_final %>% 
#   left_join(pred %>% select(text_id, descr2mine = description, text_mining_stat = predictions_raw), by = c("text_id", "descr2mine"))

df_crs_final <- df_crs_original %>%
  left_join(pred %>% select(text_id, descr2mine = description, text_mining = predictions_raw), by = c("text_id", "descr2mine")) 

names(df_crs_final)[names(df_crs_final) == "text_mining"] <- paste0("text_mining_", class_type)

# Save prediction results
saveRDS(df_crs_final, file = paste0("./Output/", class_type, "/", lang, "_classification_results.rds"))
write.xlsx(df_crs_final, file = paste0("./Output/", class_type, "/", lang, "_classification_results.xlsx"), rowNames = FALSE)


#---------------------------- Visualization  -----------------------------------

pred <- pred %>%
  left_join(df_crs_original %>% select(text_id, donorname), by = "text_id") %>%
  distinct()

df <- df %>%
  left_join(df_crs_original %>% select(text_id, donorname), by = "text_id") %>%
  distinct()

# Plot accuracy and precision trajectories
if (plot_results) source("./Code/05.1 visualize_XGBoost_results.R")



