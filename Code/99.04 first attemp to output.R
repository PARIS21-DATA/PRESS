
rm(list = ls())
# load("data/analysis/press2021_v20210928_donorDup_reemoved.rds")
# load("data/analysis/press2021_v20210928_annex_gendermarker.rds")
rm(press)
df_press <- read_rds("Data/Intermediate/99.03 crs_press_imf_2021.rds")

df_reporter_shorted <- read_rds("data/auxiliary/reporters_shorten_2022.rds") %>% unique

df_reporter_types <- read_rds("data/auxiliary/reporters_types_2022.rds")
df_reporter <- df_press %>% 
  select(ReporterId, ch_name) %>% 
  unique

df_reporter <- df_reporter %>% 
  inner_join(df_reporter_types) %>% 
  select(-ReporterName) %>% 
  left_join(df_reporter_shorted) %>% 
  mutate(ch_name = ifelse(is.na(short_names), ch_name, 
                          paste0(short_names,  " - ", ch_name)) ) %>% 
  select(-short_names)

df_press <- df_press %>% 
  select(-ch_name) %>% 
  inner_join(df_reporter)
rm(df_reporter_shorted, 
   df_reporter, 
   df_reporter_types)
# df_press %>% 
#   filter(is.na(ReporterId)) %>% 
#   slice(1:5)


# press = press %>%
#   filter(drop == 0, drop_c2.17 == 0, ! db_ref %in% lines_to_drop, !arms) 
# rm(lines_to_drop)


# 
# press = press %>%
#   select(-gentext) %>%
#   join(gender_marker)
# rm(gender_marker)

# press %>% group_by(commitmentdate) %>%
#   filter(commitmentdate > 2009) %>%
#   dplyr::summarise(total = sum(usd_commitment_defl, na.rm = T), 
#                    disb = sum(usd_disbursement_defl, na.rm =T))  %>%
#   print

# write.csv(press, file = "analysis/press2021_full.csv", row.names = F)

# names(df_press ) %>% write.csv("data/Analysis/names_press_2022.csv", row.names = F)
names_to_use = read.csv("data/Analysis/names_press_v5.csv", stringsAsFactors = F)


# press %>% select(source) %>% table()

# press %>%
#   filter(source == "CRS") %>%
#   select(commitmentdate) %>%
#   table


cols_to_sel = names_to_use %>%
  filter( New!= "") %>%
  .$original %>%
  unique
cols_to_sel

# df_press$flowname %>% table

# df_press$financing_approach %>% head
# df_press_reduced <- df_press
# df_press_reduced <- df_press %>%
#   rename(objectives = longdescription, 
#          reportedyear = year, 
#          # gentext = gender_filter_both_rmnch, 
#          currency = currencycode, 
#          commitment = commitment_national) %>% 
#   mutate( donor_type = NA, 
#           end.actual = endyear, 
#           financing_instruments = flowname) %>% 
#   select(cols_to_sel, usd_disbursement_defl)

col_names_final = names_to_use %>%
  filter( New!= "") %>%
  .$New %>%
  unique

names_to_use %>%
  filter( New!= "") %>% 
  arrange(New)




# save(press, file = "Output/press_for_ch_20221012.rds")

# press_reduced$RecipientCode %>% unique

