rm(list = ls())
crs_path <- "./Data/intermediate/crs03.rds"
crs_path_new <- "./Data/intermediate/positive_text_id.rds"
lang <-  "en"
language <- "english"
df_crs <- readRDS(crs_path)
df_crs <- df_crs %>%
  filter(is.na(description_comb) == FALSE) %>%
  select(text_id, description = description_comb, stats_filter = text_detection_wo_mining_w_scb)
df_crs_original <- df_crs

start_time = Sys.time()
source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")

# enthreshold = 30
# frthreshold = 5
# esthreshold = 1

dict_lang <- wlistV(lang) %>% as.character()

# Split into to two groups

df_crs_1 <- df_crs %>%
  filter( stats_filter 
          # , language==lang
          ) %>%
  distinct

df_crs_0 <- df_crs %>%
  filter( !stats_filter 
          # , language==lang
          )%>%
  distinct()

# which(df_crs_0$text_id %in% df_crs_1$text_id)
## 3.b. merge back by projectID to reverse previous split of project by language
# 
# #??? Why not remove all rows with empty "text" first?
# df_crs_1 <- df_crs_1 %>%
#   group_by(text_id) %>%
#   summarise(description = paste(text, collapse=". ")) %>%
#   data.frame
# df_crs_0 <- df_crs_0 %>%
#   group_by(text_id) %>%
#   summarise(description = paste(text, collapse=". ")) %>%
#   data.frame

# Warning messages:
#   1: In get(object, envir = currentEnv, inherits = TRUE) :
#   restarting interrupted promise evaluation
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation

## (3.b. english language only)



## 3.c. Pre-process documents
## es=2, fr=5, en=40
# Min.1 <- enthreshold/nrow(df_crs_1) ## only consider words that are in >5 SCB projects
# Min.1 <- Min.2
Min.1 <- 0.1 ## only consider words that are in more than 10% of statistical projects
Min.0 <- 0.1

tf_idf_keywords <- readRDS("./Tmp/tf_idf_keywords_updated.rds")

corpus_crs_1 <- preprocessingV(df_crs_1$description, language=language)
dtm_crs_1 <- DTM(corpus_crs_1, Min=Min.1, Max=1)
#myDict <- dtm_crs_1$dimnames$Terms 
myDict <- tf_idf_keywords

# myDict = unique(c(myDict, dict_lang))

## (3.c. english language only)
# save(dtm_crs_1, file = "dtm_crs_1_en_2021.Rds")

corpus_crs_0 <- preprocessingV(df_crs_0$description, language=language)
# inspect(corpus_crs_0)
nwords0 = tidy(corpus_crs_0) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)
# nwords[which(nwords != nwords0 )] 
# nwords0[which(nwords != nwords0 )]
dtm_crs_0 <- DTM(corpus_crs_0, dict = myDict)
# list_high_freq_words_0 <- DTM(corpus_crs_0 , dict = dict_lang)
# a <- tidy(list_high_freq_words_0)
# a <- list_high_freq_words_0$dimnames$Docs
list_high_freq_words_0 <- DTM(corpus_crs_0 , Min=Min.1, Max=1)$dimnames$Terms %>% unique
myDict = myDict[!(myDict %in% list_high_freq_words_0)]

# Use idf keywords
# tf_idf_keywords <- readRDS("./Data/tf_idf_keywords.rds")
myDict = unique(c(tf_idf_keywords, dict_lang))

# Frequency how often words present in myDict appear in each document, count = total appearances
freq = DTM(corpus_crs_1, dict =myDict) %>%
  tidy %>%
  group_by(document) %>%
  summarise(count = sum(count))

# Count all words in corpus 1 
#??? PROBLEM: "\\S+" counts all sequences of at leat 1 none whitespaces, are all punctuation marks removed, abbreviations (e.g. ex ...)?
nwords1 = tidy(corpus_crs_1) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>% # count all words 
  select(-text)

# Add fraction of stat words of total amount of words 
nwords1 = nwords1 %>%
  left_join(freq) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage = count/total) 

# Determine freshhold as mean fraction of statistical words in stat corpus 
threshold = nwords1 %>%
  filter(count > 0) %>%
  .$percentage %>%
  mean

print("mean:")
mean(nwords1$percentage) %>% print()
print("median:")
median(nwords1$percentage) %>% print()

# 
# x = freq %>% row_sums()
# x1 = freq$i
# x2 = freq$j
# x3 = freq$v
# x4 = freq$nrow
# x5 = freq$ncol
# x6_1 = freq$dimnames$Docs
# x6_2 = freq$dimnames$Terms


dtm_crs_0 <- DTM(corpus_crs_0, dict=myDict)

list_identified = tidy(dtm_crs_0) %>%
  group_by(document) %>%
  summarise(count = sum(count)) %>% # summ all occurances of stat words in each document
  inner_join(nwords0) %>% # nwords0 contains total number of words in description for each document
  mutate(percentage  = count/total) %>%
  filter(percentage > threshold) %>%
  .$document %>%
  as.numeric

positive_text_id = df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  filter(document %in% list_identified) %>%
  .$text_id 
  
#!!! ADDED: comparison project descriptions, stat word count
list_identified_df = tidy(dtm_crs_0) %>%
  group_by(document) %>%
  summarise(count = sum(count)) %>% # summ all occurances of stat words in each document
  inner_join(nwords0) %>% # nwords0 contains total number of words in description for each document
  mutate(percentage  = count/total) %>%
  mutate(dtm_match = ifelse(percentage > threshold, TRUE, FALSE)) %>%
  #filter(percentage > threshold) %>%
  mutate(document = as.integer(document)) %>%
  select(document, count)

positive_text_id_comp = df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  filter(document %in% list_identified) %>%
  select(text_id, document, description) %>%
  left_join(list_identified_df, by = "document")
  
df_corpus_crs_0 <- tidy(corpus_crs_0) %>%
  select(document = id, text) %>% 
  mutate(document = as.integer(document)) %>%
  left_join(positive_text_id_comp, by = "document") %>%
  mutate(dtm_match = ifelse(is.na(text_id), FALSE, TRUE))

df_crs_0_hist <- df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  inner_join(nwords0 %>% mutate(document = as.integer(document)), by = "document") %>%
  left_join(list_identified_df, by = "document") %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage  = count/total) %>%
  mutate(dtm_match = ifelse(percentage > threshold, TRUE, FALSE)) 

df_detected_tf_keywords <- df_crs_0_hist


# Save data frame of protected projects temporarily
#saveRDS(df_detected_tf_keywords, file = "./Tmp/detected_tf_keywords.rds")
#saveRDS(df_detected_high_freq_words, file = "./Tmp/detected_high_frequency_words.rds")



#-------------------------- Histograms -----------------------------------------

# Histograms of word distributions
hist_word_count_distr <- ggplot(df_crs_0_hist, aes(x = total, fill = dtm_match)) + 
  geom_histogram(binwidth = 2) + 
  xlab("Number of words in description combination") +
  ylab("Number of documents") + 
  ggtitle("Word distribution with binwidth 2 for all documents")
ggsave("./Tmp/word_distribution_all_docs_only_long_idf_keywords.pdf", width = 9, height = 7)
hist_word_count_zoom <- ggplot(df_crs_0_hist, aes(x = total, fill = dtm_match)) + 
  geom_histogram(binwidth = 1) + 
  xlim(0,50) + 
  xlab("Number of words in description combination") +
  ylab("Number of documents") + 
  ggtitle("Word distribution with binwidth 1 for all documents")
ggsave("./Tmp/word_distribution_zoom_x_all_docs_only_long_idf_keywords.pdf", width = 7, height = 7)
hist_word_count_zoom_y <- ggplot(df_crs_0_hist, aes(x = total, fill = dtm_match)) + 
  geom_histogram(binwidth = 1) + 
  coord_cartesian(
    ylim = c(0, 20)) +
  xlab("Number of words in description combination") +
  ylab("Number of documents") + 
  ggtitle("Word distribution with binwidth 1 for all documents")
ggsave("./Tmp/word_distribution_zoom_y_all_docs_only_long_idf_keywords.pdf", width = 10, height = 5)

#library(openxlsx)
#openxlsx::write.xlsx(df_corpus_crs_0, file = paste0(getwd(), "/Tmp/dtm_results_comp.xlsx"), rowNames = FALSE)


saveRDS(positive_text_id, file = crs_path_new)

print(Sys.time() - start_time)
# time spent for 1/40 of projects: almost 2 minutes