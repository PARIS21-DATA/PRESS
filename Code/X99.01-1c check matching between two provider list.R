
names(df_reporters)
names(df_survey_reporters)



a <- df_reporters %>% 
  select(z = ReporterId, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = ReporterId, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique


df_reporters_mergeBy_ReporterId <- inner_join(a, 
                                              b)

a <- df_reporters %>% 
  select(z = crs_code, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = donorcode_unified, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique

df_reporters_mergeBy_codeUnicode <- inner_join(a, 
                                               b)

a <- df_reporters %>% 
  select(z = crs_code, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = donorcode, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique

df_reporters_mergeBy_codecode <- inner_join(a, 
                                            b)



a <- df_reporters %>% 
  select(z = crs_code, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = ReporterId, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique

df_reporters_mergeBy_codeid <- inner_join(a, 
                                          b)


a <- df_reporters %>% 
  select(z = ReporterId, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = donorcode_unified, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique

df_reporters_mergeBy_idUnicode <- inner_join(a, 
                                             b)


a <- df_reporters %>% 
  select(z = ReporterId, ch_name) %>% 
  filter(!is.na(z)) %>% 
  unique
b <- df_survey_reporters %>% 
  select(z = donorcode, donorname_survey) %>% 
  filter(!is.na(z)) %>% unique

df_reporters_mergeBy_idcode <- inner_join(a, 
                                          b)
df_survey_reporters$donorname_survey %>%  unique %>% length


y = rbind(#df_reporters_mergeBy_codeid, 
  df_reporters_mergeBy_idUnicode,
  df_reporters_mergeBy_codecode,
  df_reporters_mergeBy_codeUnicode# ,
  # df_reporters_mergeBy_idcode
  # ,df_reporters_mergeBy_ReporterId
) %>% 
  select(-z) %>% 
  unique




y$donorname_survey %>% unique %>%  length

y <- y %>% 
  mutate(id = row_number())

y_reporters <- y %>% inner_join(df_reporters)
y_reporters <- y_reporters %>% 
  mutate(ReporterId_chlist = ifelse(is.na(ReporterId), crs_code, ReporterId))
y_reporters <- y_reporters %>% 
  select(ReporterId_chlist, donorname_survey, ch_name, id) %>% 
  filter(!is.na(ReporterId_chlist)) %>% #!!! very important step
  unique

y_reporters_reporters_press <- y_reporters %>% 
  inner_join(df_survey_reporters) %>% 
  rename(ReporterId_survey = donorcode_unified) %>% 
  # select(ReporterId_chlist, ReporterId_survey, 
  #        donorname_survey, ch_name, 
  #        donorname_unified) %>% 
  unique %>% 
  mutate(equal = ReporterId_survey == ReporterId_chlist) 

which(!y_reporters_reporters_press$equal )

y_reporters_reporters_press <- y_reporters_reporters_press %>% 
  select(-id, 
         -equal) 

y_reporters_reporters_press <- y_reporters_reporters_press %>% 
  rename(ReporterId_surveyOriginal = ReporterId , 
         donorname_surveyOriginal  = donorname)

names(y_reporters_reporters_press)
  
write_rds(y_reporters_reporters_press, file = "Data/auxiliary/reporters_survey_2022.rds")

names(df_crs)

m <- df_crs %>% select(1:12) %>% unique %>% 
  mutate(ReporterId_chlist = ifelse(is.na(ReporterId), donorcode, ReporterId)) %>% 
  rename(crs_code = donorcode, 
         ReporterName_chlist = ReporterName) %>% 
  select(-ReporterId) %>% 
  unique

names(m)

write_rds(m, file = "data/auxiliary/reporters_crs_2022.rds")


df_reporters_crs <- read_rds( "data/auxiliary/reporters_crs_2022.rds")
df_reporters_survey <- read_rds("Data/auxiliary/reporters_survey_2022.rds")
df_reporters2021 <- read_csv("Data/Analysis/ReporterList202112.csv")

a <- inner_join(df_reporters_crs, df_reporters2021, 
                by = c("ReporterId_chlist" = "ReporterId")) %>% 
  mutate(name_equal_RN = ReporterName == ReporterName_chlist, 
         name_equal_CN = ReporterName == ch_name)
table(a$name_equal_CN)
table(a$name_equal_RN)


b <- inner_join(df_reporters_survey, df_reporters2021, 
                     by = c("ReporterId_survey" = "ReporterId")) %>% 
  mutate(name_equal_DN = ReporterName == donorname_survey, 
         name_equal_CN = ReporterName == ch_name)
table(b$name_equal_CN)

# basically means we can trust ch_name

names(df_reporters_concat_names)
names(df_reporters_crs)

a <- df_reporters_crs %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ch_name" = "current_names"))
# only 4
a1 <- df_reporters_crs %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ch_name" = "full_names_unified"))
# only 12

a <- df_reporters_crs %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ReporterName_chlist" = "current_names"))


a <- df_reporters_survey %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ch_name" = "current_names"))
# only 4
a2 <- df_reporters_survey %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ch_name" = "full_names_unified"))
# only 12

a <- df_reporters_survey %>% 
  inner_join(df_reporters_concat_names, 
             by = c("ReporterName_chlist" = "current_names"))


c <- df_reporters_concat_names %>% 
  inner_join(df_reporters, 
             by = c( "full_names_unified" = "ch_name"))
d <- which(!df_reporters_concat_names$full_names_unified %in% c$full_names_unified)
d <- which(!df_reporters_concat_names$full_names_unified %in% df_reporters$ch_name)
df_reporters_concat_names[d,] %>% as.data.frame

e <- df_reporters_concat_names[d,] %>% 
  inner_join(df_reporters_survey, 
             by = c( "short_names" = "ch_name"))

d1 <- which(!df_reporters_concat_names[d,]$short_names %in% e$short_names)

e1 <- df_reporters_concat_names[d,][d1,] %>% 
  inner_join(df_reporters_crs, 
             by = c( "short_names" = "ch_name"))

d2 <- which(!df_reporters_concat_names[d,][d1,]$short_names %in% e1$short_names)

e2 <- df_reporters_concat_names[d,][d1,][d2,] %>% 
  inner_join(df_reporters, 
             by = c( "short_names" = "crs_name_en"))

d3 <-  which(!df_reporters_concat_names[d,][d1,][d2,]$short_names %in% e2$short_names)

e3 <- df_reporters_concat_names[d,][d1,][d2,][d3,] %>% 
  inner_join(df_reporters_survey, 
             by = c("current_names" = "donorname_survey"))


d4 <-  which(!df_reporters_concat_names[d,][d1,][d2,][d3,]$current_names %in% e3$current_names)

e4 <- df_reporters_concat_names[d,][d1,][d2,][d3,][d4,] #%>% 
  # inner_join(df_reporters_crs, 
             # by = c("current_names" = "ch_name"))
c_reduced <- c %>% select( short_names, ReporterId, crs_code) %>% 
  mutate(ReporterId_chlist = ifelse(is.na(ReporterId), crs_code, ReporterId)) %>% 
  select(-ReporterId, -crs_code) %>% 
  unique %>% 
  rename(ReporterId = ReporterId_chlist)
e_reduced <- e %>% select( short_names, ReporterId_survey) %>% unique %>% 
  rename(ReporterId = ReporterId_survey)
e1_reduced <- e1  %>% select( short_names, ReporterId_chlist) %>% unique %>% 
  rename(ReporterId = ReporterId_chlist)
e2_reduced <- e2 %>% select( short_names, ReporterId, crs_code)%>% 
  mutate(ReporterId_chlist = ifelse(is.na(ReporterId), crs_code, ReporterId)) %>% 
  select(-ReporterId, -crs_code) %>% 
  unique %>% 
  rename(ReporterId = ReporterId_chlist)
e3_reduced <- e3 %>% select( short_names, ReporterId_survey) %>% 
  rename(ReporterId = ReporterId_survey)

ce <- rbind(c_reduced, 
            e_reduced,
            e1_reduced, 
            e2_reduced, 
            e3_reduced) %>% 
  unique

e4_mod <- df_reporters_concat_names %>% 
  filter(!short_names %in% ce$short_names)



write_rds(ce, file = "data/auxiliary/reporters_shorten_2022.rds")
write_rds(e4_mod , file = "Data/auxiliary/reporters_shortenLeftover_2022.rds")


l <- df_reporters_press %>% 
  select(ReporterId, donor_type) %>% 
  unique

t <- df_reporters_survey %>% 
  select(ReporterId_surveyOriginal) %>% 
  unique

lt <- inner_join(l, t, 
                 by = c("ReporterId" = "ReporterId_surveyOriginal"))


df_reporters_survey1 <- lt %>% 
  rename(ReporterId_surveyOriginal = ReporterId) %>% 
  inner_join(df_reporters_survey)

write_rds(df_reporters_survey1, 
          file = "Data/auxiliary/reporters_survey_2022.rds")



