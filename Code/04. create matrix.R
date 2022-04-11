rm(list = ls())
crs_path <- "./Data/intermediate/crs03_1.rds"
crs_path_new <- "./Data/intermediate/positive_text_id.rds"
lang <-  "en"
language <- "english"
df_crs <- readRDS(crs_path)
df_crs_original <- df_crs

start_time = Sys.time()
source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")

# enthreshold = 30
# frthreshold = 5
# esthreshold = 1

dict_lang <- wlistV(lang)

# Split into to two groups

df_crs_1 <- df_crs %>%
  filter( stats_filter 
          # , language==lang
          )

df_crs_0 <- df_crs %>%
  filter( !stats_filter 
          # , language==lang
          )
# which(df_crs_0$text_id %in% df_crs_1$text_id)
## 3.b. merge back by projectID to reverse previous split of project by language

df_crs_1 <- df_crs_1 %>% 
  group_by(text_id) %>% 
  summarise(description = paste(text, collapse=". ")) %>%
  data.frame
df_crs_0 <- df_crs_0 %>% 
  group_by(text_id) %>% 
  summarise(description = paste(text, collapse=". ")) %>%
  data.frame
# 
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

corpus_crs_1 <- preprocessingV(df_crs_1$description, language=language)
dtm_crs_1 <- DTM(corpus_crs_1, Min=Min.1, Max=1)
myDict <- dtm_crs_1$dimnames$Terms 

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

list_high_freq_words_0 <- DTM(corpus_crs_0 , Min=Min.1, Max=1)$dimnames$Terms %>% unique
myDict = myDict[!(myDict %in% list_high_freq_words_0)]
myDict = unique(c(myDict, dict_lang))

freq = DTM(corpus_crs_1, dict =myDict) %>%
  tidy %>%
  group_by(document) %>%
  summarise(count = sum(count))

nwords1 = tidy(corpus_crs_1) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)

nwords1 = nwords1 %>%
  left_join(freq) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage = count/total) 

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
  summarise(count = sum(count)) %>%
  inner_join(nwords0) %>%
  mutate(percentage  = count/total) %>%
  filter(percentage > threshold) %>%
  .$document %>%
  as.numeric

positive_text_id = df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  filter(document %in% list_identified) %>%
  .$text_id 


saveRDS(positive_text_id, file = crs_path_new)

print(Sys.time() - start_time)
# time spent for 1/40 of projects: almost 2 minutes