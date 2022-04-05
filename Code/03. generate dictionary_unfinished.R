
rm(list = ls())

crs_path <- "./Data/intermediate/crs02.rds"
df_crs_raw <- readRDS(crs_path)

df_crs <- df_crs_raw
# beep(1)
# gc()
# this is to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes

# df_crs <- df_crs %>%
#   group_by(projectID) %>%
#   dplyr::summarise(SCB = mean(SCB), POP = mean(POP)) %>%
#   mutate(SCB = ifelse(SCB > 0, 1, 0), 
#          POP = ifelse(POP > 0, 1, 0)) %>%
#   merge(df_crs[,!names(df_crs) %in% c("SCB", "POP")], by = "projectID")

# df_crs <- group_by(df_crs, projectID)

## 1.b. drop duplicate project descriptions 
df_crs <- df_crs[!duplicated(df_crs$text_id),]


## 1.c. split projects by language delimiters "." and " / "
# df_crs <- as.data.frame(df_crs)
names(df_crs)
# beepr::beep(3)

# df_crs$description_comb <- tolower(df_crs$description_comb)
# df_crs$description_comb[457]

# df_crs$toDetect = df_crs$description_comb 

# df_crs$description_comb = iconv(df_crs$description_comb,"WINDOWS-1252","UTF-8")
df_crs$description_comb <- tolower(df_crs$description_comb)


# substr(df_crs$description, 3, nchar(df_crs$description))

# length(which(is.na(df_crs$toDetect)))


which(is.na(df_crs$description_comb)) %>% print 
which(df_crs$description_comb == "") %>% print

# language1 <- detectLanguage(df_crs$toDetect, isPlainText=TRUE)$detectedLanguage
# language2 = cld2::detect_language(df_crs$toDetect)
# language3 = cld3::detect_language(df_crs$toDetect)

# table(language1)

df_crs = df_crs %>%
  mutate(language = cld2::detect_language(longdescription))

table(df_crs$language) %>% print 

save(df_crs, file = crs_path_new)

# load("./analysis/crs_2021_clean0.5_withLangDetect.Rds")


# crs$description_comb = NULL
# names(crs)
# crs <- cSplit(crs, "toDetect", ".", "long")
# crs <- cSplit(crs, "toDetect", " / ", "long")
# save(crs, file = "crs_2020_clean1_splitToDetect.RData")



############
# preselect the definitely statistics project 

# load("crs_2021_clean1_splitted.RDs")
rm(list = ls())

load("./analysis/crs_2021_clean0.5_withLangDetect.Rds")
df_crs_o = df_crs
names(df_crs)

# beep(4)
df_crs$data = grepl("statis|estadi", df_crs$projecttitle, ignore.case = T)
# df_crs$SCB = ifelse((df_crs$purposecode == 16062 ) & (!is.na(df_crs$purposecode)), 1, 0)
# df_crs$POP = ifelse(( df_crs$purposecode  ==13010 ) & (!is.na(df_crs$purposecode)), 1, 0)
# df_crs$data2 = grepl("data|datos|donne", df_crs$projecttitle, ignore.case = T)

df_crs$DHS = grepl("DHS", df_crs$projecttitle, ignore.case = T)

df_crs$census = grepl("census|Volkszählung|recensement|censo", df_crs$projecttitle, ignore.case = T)
df_crs$survey = grepl("survey|sondage|enquête|encuesta", df_crs$projecttitle, ignore.case = T)
df_crs$mining = grepl("land mine|small arm|demining|demine|landmine", df_crs$projecttitle, ignore.case = T)

df_crs$stats = df_crs$data | df_crs$census | df_crs$survey | (df_crs$SCB==1) | df_crs$DHS# | ( df_crs$data2) 

df_crs$stats = df_crs$stats & (!df_crs$mining)

table(df_crs$stats)


df_crs = join(df_crs_o, df_crs[,c("projectID", "stats")], by="projectID")
df_crs = df_crs %>%
  mutate(stats = ifelse(is.na(stats), FALSE, stats))
# tail(df_crs)
table(df_crs$stats)
rm(df_crs_o)

# beep(4)

# crs = crs %>%
#   slice(-which(is.na(crs$description_comb)))
# 
# which((crs$description_comb == " ")) %>% length
# head(crs$description_comb,100)

df_crs_stats = df_crs %>% select(projectID, stats) %>% unique 
which(duplicated(df_crs_stats$projectID))

save(df_crs,file = crs_path_new)
###############
# not very useful 
################
# load("crs_2019_clean1.RData")
# 
# crs$description_comb <- tolower(crs$description_comb)

# language <- detectLanguage(crs$description, isPlainText=TRUE)
# nc <- c("ENGLISH","FRENCH","SPANISH")
nc <- c("en","fr","es")
df_crs = df_crs%>%select(-toDetect)
df_crs = df_crs %>%
  mutate(language = ifelse(language %in% nc, language, "other"))
# rm(language,nc)
# table(df_crs$language)
# save(df_crs, file="crs_2019_clean2.RData")

################


#Output::
save(df_crs, file="./analysis/df_crs_2021_clean3_notCoverted2ascii.RDs")

# load("./analysis/crs_2021_clean3_notCoverted2ascii.RDs")
df_crs$description_comb = iconv(df_crs$description, 'utf-8', 'ascii', sub=' ')
save(df_crs, file="./analysis/df_crs_2021_clean3_Coverted2ascii.RDs")
df_crs <- cSplit(df_crs, "description_comb", ".", "long")
df_crs <- cSplit(df_crs, "description_comb", " / ", "long")

df_crs$description_comb = tolower(df_crs$description_comb)
head(df_crs$description_comb)
# crs$description_comb = iconv(crs$description, 'utf-8', 'ascii', sub=' ')
# save(crs, file="./analysis/crs_2021_clean3_Coverted2ascii.RDs")
# 
# 
# gc()
# load("./analysis/crs_2021_clean3_Coverted2ascii.RDs")

# load("./analysis/crs_2021_clean3_notCoverted2ascii.RDs")

# crs <- cSplit(crs, "description_comb", ".", "long")
# crs <- cSplit(crs, "description_comb", " / ", "long")



# ?cSplit
save(df_crs, file="./analysis/crs_2021_clean2.9_converted_splitted.RDS")

load("./analysis/crs_2021_clean2.9_converted_splitted.RDS")

df_crs = df_crs %>% select(-stats) %>% join(df_crs_stats)
projectIDs = df_crs_stats %>% filter(stats) %>% .$projectID 


save(df_crs, file="./analysis/crs_2021_clean3_converted_splitted.RDS")
save(projectIDs, file = "analysis/projectIDs_by_title.rds")
# head(crs)

beepr::beep(3)

rm(list = ls())
gc()
