rm(list = ls())
gc()
Sys.sleep(10)
source("code/00. boot.R")
source("code/00.1a functions_stem_and_concat.R")

job_specific_suffix <- "_full_"
path_input_crs <- paste0("./Data/intermediate/crs02 new columns", job_specific_suffix, year(Sys.Date()), ".feather")
path_output_intermediate_crs <- paste0("./Data/intermediate/crs03_intermediate_", job_specific_suffix, year(Sys.Date()), ".feather")
path_output_crs <- paste0("./Data/intermediate/crs03", job_specific_suffix, year(Sys.Date()), ".feather")
start <- Sys.time()
df_crs_full <- read_feather(path_input_crs)
print("Load file:")
print_time_diff(start)
# Time difference of 24.98078 secs

df_crs_full <- df_crs_full %>%
  mutate(projecttitle_lower = tolower(projecttitle)) # %>%
  # mutate(title_id = as.numeric(as.factor(projecttitle_lower))) 

### we used to make the project with same description as 1 as long as one of the same description is marked as 1
### it is wrong because some projects with the same name will have different purpose codes

## 1. split projects by language delimiters "." and " / "

# names(crs)
# crs <- cSplit(crs, "toDetect", ".", "long")
# crs <- cSplit(crs, "toDetect", " / ", "long")

## convert project to lower cases to assist text search
df_crs_reduced <- df_crs_full %>%
  select(title_id, projecttitle_lower, language_title) %>%
  filter(!duplicated(title_id)) 
  
langs = c("en",
           "fr"
          , "es"
          , "de"
          )
print_time_diff(start)
# Time difference of 56.87824 secs

# lang2analyse <- "fr"
start <- Sys.time()
list_df_crs <- list()
for (i in 1:length(langs)) {
  lang2analyse <- langs[i]
  source(file = "Code/03.2 detecting for each language.R")
  list_df_crs[[i]] <- df_crs
}
print_time_diff(start)
df_crs <- bind_rows(list_df_crs)
rm(list_df_crs)
print_time_diff(start)
gc()
beep()
## joining the filtering results back with the full version of data
df_crs <- df_crs_full %>% 
  left_join(df_crs) %>%
  select(-projecttitle_lower)
beep()

df_crs %>% 
  write_feather(path_output_intermediate_crs)

# remive NAs in each column, it seems that the results for gender identification changed a lot while the results for data & stats didn't. 
names(df_crs)
gc()
df_crs <- df_crs %>% 
  mutate(scb = (scb == 1), 
         pop = (pop == 1)) %>% 
  mutate(across(scb:gen_rmnch2,  ~ replace_na(.x, F))) %>% 
  mutate(mining_ppcode = ifelse(is.na(mining_ppcode), 
                                F, 
                                mining_ppcode)) %>% 
  mutate(across(stat_title:mining_title, ~ replace_na(.x, F) ))



names(df_crs)

gc()

# attributes(df_crs$text_detection)$description = "title contains keywords for statistics"
# attributes(df_crs$text_detection_gender)$description = "title contains keywords for gender"
# attributes(df_crs$gen_marker1)$description = "gender marker equals 1"
# attributes(df_crs$gen_marker2)$description = "gender marker equals 2"
# attributes(df_crs$gen_marker)$description = "gender marker equals 1 or 2"

source("code/03.3 adding missing columns manually.R")

df_crs <- df_crs %>%
  mutate(stat_title_ex_mining = (stat_title|stat_manual_additions) & !mining_title 
  ) %>% 
  mutate(text_detection_wo_mining = stat_title_ex_mining)
# attributes(df_crs$text_detection_wo_mining)$description = "title contains keywords for statistics, but not keywords for demining and small arms"
df_crs %>% 
  filter(is.na(stat_title_ex_mining))

# langues <- c("en","fr","es")
# df_crs <- df_crs %>%
#   select(-projecttitle_lower) %>%
#   mutate(language = ifelse(language %in% langues, language, "other") )

df_crs <- df_crs %>%
  mutate(stat_title_ppcode = (stat_title_ex_mining | scb) & (!mining_ppcode))

# attributes(df_crs$stats_filter)$description = "title contains keywords for statistics or purposecode 16062, but not keywords for demining and small arms"

table(df_crs$stats_title_ex_mining) %>% print
table(df_crs$stats_title_ppcode) %>% print
# which(is.na(df_crs$text_detection_wo_mining_w_scb))
print_time_diff(start)

df_crs <- df_crs %>%
  mutate(gen_markers_title = gen_donor|gen_ppcode|gen_title|gen_marker2|gen_channel|gen_agency|gen_sdg|gen_rmnch2)  




# legacy columns
df_crs <- df_crs %>% 
  mutate(# mining , 
    text_detection = stat_title, 
    text_detection_gender = gen_title)
df_crs <- df_crs %>%
  mutate(text_filter_gender  =  gen_markers_title)
df_crs <- df_crs %>% 
  mutate(stats_filter = stat_title_ppcode)



  # mutate(text_filter_gender_narrower = gen_ppcode|text_detection_gender|gen_donor
  #        # | gen_marker2
  # ) 
# table(df_crs$text_filter_gender_narrower)

# attributes(df_crs$text_filter_gender)$description = "identified as a gender project before text mining, use gender marker ==2 only. This is different from text_detection_gender!!"
# attributes(df_crs$text_filter_gender_narrower)$description = "identified as a gender project before text mining, do not use gender marker at all. This is different from text_detection_gender!!"

a = df_crs %>% select(text_id,stats_filter, text_detection_gender) %>% unique %>% nrow
b = df_crs %>% select(text_id) %>% unique %>% nrow
print_time_diff(start)

print(paste0("There are ", a-b, " projects with same names but different purpose code"))

names(df_crs)

# str_rm_nul <- function(x) {
#   if(!is.character(x)) {return(x)} else{
#     x <- str_replace(x, "\200", "")
#     return(x)
#   }
# }
# 
# df_crs_rm_nul <- lapply(df_crs, str_rm_nul)

# df_crs <- data.frame(df_crs_rm_nul, stringsAsFactors = F)
# rm(df_crs_rm_nul)

write_feather(df_crs,path_output_crs)

print_time_diff(start)
# Time difference of 134.7109 secs
# write.csv(df_crs, file = "data/intermediate/crs_filter_results.csv", row.names = F)
# df_crs %>%
#   filter(text_detection_wo_mining_w_scb, text_detection_gender # |gen_marker2
#          ) %>%
#   nrow
