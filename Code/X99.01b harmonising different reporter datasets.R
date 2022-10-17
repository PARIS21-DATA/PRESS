
#######
# 2.31
# check compatability between CRS and survey

which(!df_reporters_press$donorname_unified  %in% df_reporters$ReporterName )
which(!df_reporters_press$donorname_unified  %in% df_reporters$crs_name_en )
which(!df_reporters_press$donorname_unified  %in% df_reporters$ch_name)
which(!df_reporters_press$donorname_unified  %in% df_reporters$name_modification)

which(!df_reporters_press$donorname  %in% df_reporters$ReporterName )
which(!df_reporters_press$donorname  %in% df_reporters$crs_name_en ) # fully compatible!
which(!df_reporters_press$donorname  %in% df_reporters$ch_name)
which(!df_reporters_press$donorname  %in% df_reporters$name_modification)

which(!df_reporters_press$ReporterName  %in% df_reporters$ReporterName )
which(!df_reporters_press$ReporterName   %in% df_reporters$crs_name_en )
which(!df_reporters_press$ReporterName   %in% df_reporters$ch_name)
which(!df_reporters_press$ReporterName   %in% df_reporters$name_modification)


############
which(!df_survey$donorname_unified  %in% df_reporters$ReporterName )
which(!df_survey$donorname_unified  %in% df_reporters$crs_name_en )
which(!df_survey$donorname_unified  %in% df_reporters$ch_name)
which(!df_survey$donorname_unified  %in% df_reporters$name_modification)

which(!df_survey$donorname  %in% df_reporters$ReporterName )
which(!df_survey$donorname  %in% df_reporters$crs_name_en ) # fully compatible!
which(!df_survey$donorname  %in% df_reporters$ch_name)
which(!df_survey$donorname  %in% df_reporters$name_modification)

which(!df_survey$ReporterName  %in% df_reporters$ReporterName )
which(!df_survey$ReporterName   %in% df_reporters$crs_name_en )
which(!df_survey$ReporterName   %in% df_reporters$ch_name)
which(!df_survey$ReporterName   %in% df_reporters$name_modification)





vec_survey_donorcode <- df_survey %>% 
  .$donorcode %>% 
  unique
vec_survey_reporterId <- df_survey %>% 
  .$ReporterId %>% 
  unique

(!vec_survey_donorcode %in% df_reporters$ReporterId ) %>% which
(!vec_survey_donorcode %in% df_reporters$crs_code) %>% which

(!vec_survey_reporterId %in% df_reporters$ReporterId) %>% which
(!vec_survey_reporterId %in% df_reporters$crs_code) %>% which

(! df_survey$donorname %in% df_reporters$crs_name_en) %>% which
is.na(df_survey$donorname) %>% which


#######
# 2.4 checking the compatibility between press reporter list and ch reporter list 
names(df_reporters_press)
names(df_reporters)

df_reporters_4merge <- df_reporters %>% 
  select(crs_code, ReporterId, ReporterName, crs_name_en, 
         ch_name) %>% 
  unique 
df_reporters_press_4merge <- df_reporters_press %>% 
  select(ReporterId, ReporterName, donorname, donorcode, donorname_unified) %>% 
  unique %>% 
  select(-ReporterName) %>% 
  rename(ReporterName = donorname_unified)

df_reporters_merged <- inner_join(df_reporters_4merge, df_reporters_press_4merge) %>% 
  filter(!is.na(ReporterId))
# 120 if left join
# 96 if right join

df_reporters_codes <- df_reporters_merged %>% 
  select(crs_code, donorcode, ReporterId, ReporterName, ch_name) %>% 
  unique 

### 
# checking compatibility after first merge

which(!df_reporters_press$ReporterId  %in% df_reporters$ReporterId )
which(!df_reporters_press$donorcode  %in% df_reporters$ReporterId )





a <-  which(!df_reporters_press$ReporterId  %in% df_reporters$ReporterId )

df_reporters_press %>% 
  select(donorname, ReporterName) %>% 
  slice(a)

df_reporters_press_notHavingDonorName <- df_reporters_press %>% 
  filter(!is.na(ReporterName)) %>% 
  select(ReporterName, donorname, ReporterId, donorcode, donorname_unified) %>% 
  filter(is.na(donorname)) %>% 
  select(-donorname)


df_survey_reporters_unique <- df_survey %>% 
  select(donorname) %>% 
  unique

a <- (!(df_reporters_press_notHavingDonorName$ReporterId %in% df_reporters$ReporterId) )

b <- ((df_reporters_press_notHavingDonorName$ReporterId %in% df_survey_reporters_unique$donorname))

a&(b)




df_reporters_press_notHavingDonorName[a,]  %>% write_csv("data/analysis/press_reporterList_notIn_chList.csv")






which(!df_reporters_press$ReporterId  %in% df_reporters$crs_code)
which(!df_reporters_press$donorcode  %in% df_reporters$crs_code)

a <- which(!df_reporters_press$donorcode  %in% df_reporters$crs_code)

df_reporters_press %>% 
  select(donorname, ReporterName) %>% 
  slice(a)



df_reporters %>% 
  filter(is.na(crs_code), !is.na(ReporterId)) %>% 
  select(ReporterName, ch_name)

df_reporters %>% 
  filter(!is.na(crs_code), is.na(ReporterId)) %>% 
  select(ch_name)



which(!df_reporters_press$ReporterId  %in% df_reporters$ReporterId )
which(!df_reporters_press$ReporterId  %in% df_reporters$ReporterId )

which(!df_reporters_press$ReporterName  %in% df_reporters$crs_name_en )
which(!df_reporters_press$ReporterName  %in% df_reporters$ch_name)
which(!df_reporters_press$ReporterName %in% df_reporters$name_modification)


