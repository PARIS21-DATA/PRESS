source("code/00. boot.R")
pkgload:::unload("tidytext") # the stemmer in tidy text might be problematic for our steps here. 

source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")

job_specific_suffix <- "_utf8_full"
load("data/intermediate/crs04_lang_utf8_full.rdata")

crs_path <- paste0("./Data/intermediate/crs03",
                   job_specific_suffix, ".rds")
crs_path_new_1 <- paste0("./Data/intermediate/crs04.0_crs1_", lang,job_specific_suffix, ".rds")
crs_path_new_0 <- paste0("./Data/intermediate/crs04.0_crs0_", lang,job_specific_suffix, ".rds")
start <- Sys.time()

df_crs <- read_rds(crs_path)
print_time_diff(start)


# enthreshold = 30
# frthreshold = 5
# esthreshold = 1

## Whitelist is no longer being used
# Create the white list 
# dict_lang <- wlistV(lang) %>% as.character()

df_crs_1 <- df_crs %>%
  filter( stats_filter 
          , language==lang
          ) %>% 
  filter(!duplicated(text_id)) %>%
  select(description = desc_2mine, text_id)

df_crs_0 <- df_crs %>%
  filter( !stats_filter 
          , language==lang
          ) %>% 
  filter(!duplicated(text_id)) %>%
  select(description = desc_2mine, text_id)
print_time_diff(start)

write_rds(df_crs_0, file = crs_path_new_0)
write_rds(df_crs_1, file = crs_path_new_1)


# which(df_crs_0$text_id %in% df_crs_1$text_id)


## 3.b. merge back by projectID to reverse previous split of project by language
# df_crs_1 <- df_crs_1 %>% 
  # group_by(text_id) %>% 
  # summarise(description = desc_2mine) %>%
  # data.frame

# df_crs_0 <- df_crs_0 %>% 
  # group_by(text_id) %>% 
  # summarise(description = paste(text, collapse=". ")) %>%
  # data.frame
# 
# Warning messages:
#   1: In get(object, envir = currentEnv, inherits = TRUE) :
#   restarting interrupted promise evaluation
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation



## 3.c. Pre-process documents
## es=2, fr=5, en=40
# Min.1 <- enthreshold/nrow(df_crs_1) ## only consider words that are in >5 SCB projects
# Min.1 <- Min.2
