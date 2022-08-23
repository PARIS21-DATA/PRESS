
df_crs_0 <- read_rds("./Data/intermediate/crs04_crs0_utf8_full.rds")


start <- Sys.time()
x <- seq_along(df_crs_0$description)
d0 <- split(df_crs_0$description, ceiling(x/ceiling(length(df_crs_0$description)/100)))
print_time_diff(start)
rm(x)

language <- "en"
prep <- function(x, language = "en") {
  x <- preprocessingV(x, language = language)
  print(length(x))
  print_time_diff(start)
  return(x)
}
start <- Sys.time()
d1 <- lapply(d0, prep, language = language)
print_time_diff(start)
# Time difference of 683.6464 secs if divided into 10 parts
# 100 parts: Time difference of 614.0661 secs


write_rds(d1, file = "data/Intermediate/crs04.2_crs0_d1_utf8_full.rds")

dtm_batch <- function(x) {
  x <-DTM(x)
  print(length(x))
  print_time_diff(start)
  return(x)
}




start <- Sys.time()
d2 <- lapply(d1, dtm_batch)
print_time_diff(start)

detach(tidytext)


d3 <- lapply()


freq_all0 <- corpus_crs_0_simpleDTM %>% 
  tidytext::tidy() %>%
  group_by(term) %>%
  summarise(cnt = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(cnt)) %>%
  mutate(freq = cnt / total)
# beepr::beep(2)


## not time consuming
freq_all_1_0 = right_join(freq_all0, freq_all1, by = "term") %>%
  mutate(odds = freq.x/freq.y) %>%
  arrange(desc(odds), desc(freq.y))



