## ??? This is used before but should I correct after changing 00.4? 
# source("Code/00.4 refining keywords.R")
rm(list = ls())
source("code/00. boot.R")
myDict <- read_rds("data/Intermediate/crs04.2_mydict_utf8_full.rds")
corpus_crs_1 <- read_rds("./Data/intermediate/crs04.1_corpus_crs_1_utf8_full.rds")
nwords0 <- read_rds("data/Intermediate/crs04.3_nwords0_utf8_full.rds")
dtm_crs_0 <- read_rds("Data/Intermediate/crs04.3_dtm_crs_0_utf8_full.rds")
df_crs_0 <- read_rds("./Data/intermediate/crs04_crs0_utf8_full.rds")
source("code/00.3 functions_yu.R")
source("code/00.2 functions_thilo.R")

start <- Sys.time()
freq <- DTM(corpus_crs_1, dict =myDict) %>%
  tidy %>%
  group_by(document) %>%
  summarise(count = sum(count))
print_time_diff(start)
# Time difference of 10.85936 secs

nwords1 <- tidy(corpus_crs_1) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)
print_time_diff(start)

nwords1 <- nwords1 %>%
  left_join(freq) %>%
  filter(total != 0) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage = count/total) 
beep()

threshold <- nwords1 %>%
  filter(count > 0) %>%
  .$percentage %>%
  mean

print(paste("Threshold:", threshold))
print("mean:")
mean(nwords1$percentage, na.rm = T) %>% print()
print("median:")
median(nwords1$percentage, na.rm = T) %>% print()


list_identified <- tidy(dtm_crs_0) %>%
  group_by(document) %>%
  summarise(count = sum(count)) %>%
  inner_join(nwords0) %>%
  filter(total > 0) %>% 
  mutate(percentage  = count/total) %>%
  filter(percentage > threshold) %>%
  .$document %>%
  as.numeric
print_time_diff(start)

positive_text_id <- df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  filter(document %in% list_identified) %>%
  .$text_id 
beep()

df_crs_0 %>% filter(text_id %in% positive_text_id) %>% .$description %>% print
# a = df_crs_0 %>% 
#   select(text_id, description) %>% 
#   filter(text_id %in% positive_text_id)
# 
# b = df_crs_0 %>% 
#   select(text_id, description) %>% 
#   filter(text_id %in% positive_text_id1)
# 
# c = full_join(a, b, by = "text_id") %>%
#   filter(is.na(description.x)|is.na(description.y))
# c$description.x

write_rds(positive_text_id, file = "data/Intermediate/crs04.4_positive_id_utf8_full.rds")

print_time_diff(start)
rm(list = ls())
gc()
# time spent for 1/40 of projects: 1.416996 mins