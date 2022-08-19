rm(list = ls())

source("code/00.1 functions.R")

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

job_specific_suffix <- "_utf8_full"
crs_path <- paste0("./Data/intermediate/crs02", job_specific_suffix, ".rds")
crs_path_new <- paste0("./Data/intermediate/crs03", job_specific_suffix, ".rds")
start <- Sys.time()
df_crs_full <- readRDS(crs_path)
print("Load file:")
print_time_diff(start)


df_crs_full <- df_crs_full %>%
  mutate(projecttitle_lower = tolower(projecttitle)) %>%
  mutate(title_id = as.numeric(as.factor(projecttitle_lower))) 

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
  

langs = c("en"
          # , "de"
          )

for (lang2analyse in langs) {
  source(file = "Code/03.2 detecting for each language.R")
}

df_crs <- rbind(df_crs_en 
                # ,df_crs_de
                ) #!!! fix here
# df_crs <- df_crs_full %>%
#   filter(!language_title %in% langs) %>%
#   bind_rows(df_crs)

df_crs <- df_crs_full %>% 
  right_join(df_crs) %>%
  select(-projecttitle_lower)

df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = text_detection & !mining
  )


# langues <- c("en","fr","es")
# df_crs <- df_crs %>%
#   select(-projecttitle_lower) %>%
#   mutate(language = ifelse(language %in% langues, language, "other") )

df_crs <- df_crs %>%
  mutate(text_detection_wo_mining_w_scb = text_detection_wo_mining | scb)
table(df_crs$text_detection_wo_mining) %>% print
table(df_crs$text_detection_wo_mining_w_scb) %>% print
# which(is.na(df_crs$text_detection_wo_mining_w_scb))
print_time_diff(start)

df_crs <- df_crs %>%
  mutate(text_filter_gender = gen_donor|gen_ppcode|text_detection_gender| gen_marker2
  )

a = df_crs %>% select(text_id, text_detection_wo_mining_w_scb, text_detection_gender) %>% unique %>% nrow
b = df_crs %>% select(text_id) %>% unique %>% nrow
print_time_diff(start)
# list = df_crs %>% 
#   filter(text_detection_wo_mining_w_scb) %>%
#   select(text_id, projecttitle) %>%
#   unique
# saveRDS(list, file = "data/list_by_P21.rds")

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

write_rds(df_crs,file = crs_path_new)

print_time_diff(start)
# write.csv(df_crs, file = "data/intermediate/crs_filter_results.csv", row.names = F)
# df_crs %>%
#   filter(text_detection_wo_mining_w_scb, text_detection_gender # |gen_marker2
#          ) %>%
#   nrow
