setwd("~/Dropbox/PARIS21/PRESS/2021/")
rm(list = ls())
load("iati_recs_splited.rdata")


iati_recs_summary = iati_recs %>%
  filter(recipient.country != "") %>%
  group_by(db_ref, regionPRESS) %>%
  dplyr::summarise(number_of_recipients = n()) %>%
  group_by(db_ref) %>%
  mutate(number_of_regions = n()) 

# table(iati_recs_summary$number_of_regions)

iati_recs_summary = iati_recs_summary %>%
  mutate(regionPRESS = ifelse(number_of_regions>1, "Unspecified", regionPRESS)
         # , 
         # region =ifelse(number_of_regions>1, "Unspecified", region) 
         ) %>%
  mutate(countrySpecific = ifelse((number_of_recipients>1) , F, ifelse(regionPRESS!="Unspecified", T , F)))

iati_recs_summary = iati_recs_summary %>%
  select(db_ref, countrySpecific, regionPRESS) %>%
  unique

dups = iati_recs_summary$db_ref %>% duplicated %>% which
dups = iati_recs_summary$db_ref[dups]
dups = iati_recs_summary %>%
  filter(db_ref %in% dups)
rm(dups)

# iati_recs_regionSpecificMarked = iati_recs %>%
#   select(-regionPRESS) %>%
#   merge( iati_recs_summary, by = "db_ref", all.x = T) 
# 
# names(iati_recs_regionSpecificMarked)
# iati_recs_regionSpecificMarked = iati_recs_regionSpecificMarked %>%
#   select(db_ref, iso3c, regionPRESS, countrySpecific) %>%
#   mutate(iso3c = ifelse(countrySpecific,iso3c, "Multiple recipients or unspecified" )) %>%
#   unique

summary_regionSpecific = iati_recs_summary

# which(is.na(iati_recs_regionSpecificMarked$countrySpecific))

# dup_num = which(duplicated(iati_recs_regionSpecificMarked$db_ref))
# dup_num = iati_recs_regionSpecificMarked$db_ref[dup_num]
# dups = iati_recs_regionSpecificMarked %>%
#   filter(db_ref %in% dup_num)


# a = iati_recs_regionSpecificMarked %>% filter(is.na(regionPRESS)) %>% unique
# 
# rm(a)


# iati_recs = iati_recs %>% filter(!is.na(regionPRESS))
rm(iati_recs_summary)  

iati_recs_summary = iati_recs %>%
  # filter(recipient.country != "") %>%
  group_by(db_ref, DACrecipient) %>%
  dplyr::summarise(number_of_DACrecipient = n()) %>%
  group_by(db_ref) %>%
  mutate(number_of_DACcategories = n())  %>%
  ungroup %>%
  mutate(allDAC = (number_of_DACcategories <2 ) & (DACrecipient == T) ) 
# table(iati_recs_summary$allDAC)

iati_recs_summary = iati_recs_summary  %>%
  select(db_ref, allDAC) %>%
  unique

summary_allDAC = iati_recs_summary

summary = merge(summary_allDAC, summary_regionSpecific, by = "db_ref")
rm(summary_allDAC, summary_regionSpecific)
rm(iati_recs_summary)

iati_marked = merge(iati, summary, by = "db_ref", all.x = T)

iati_marked = iati_marked %>%
  mutate(regionPRESS = ifelse(is.na(regionPRESS), "Unspecified", regionPRESS), 
         countrySpecific = ifelse(is.na(countrySpecific), F, countrySpecific), 
         allDAC = ifelse(is.na(allDAC), F, allDAC))

names(iati_marked)



iati_list = iati_marked %>%
  select(db_ref, countrySpecific)

iati_recs_grouped = iati_recs %>%
  select(db_ref, iso3c) %>%
  unique %>%
  group_by(db_ref) %>%
  summarise(iso3c = paste0(iso3c, collapse = ";")) 



iati_recs_grouped_all = iati_list %>%
  merge(iati_recs_grouped, by = "db_ref", all.x = T) %>%
  mutate(iso3c = ifelse(is.na(iso3c), "Unspecified", iso3c), iso3c = ifelse(countrySpecific, iso3c, "Unspecified"))


iati_marked_isos = merge(iati_marked, iati_recs_grouped_all, by = "db_ref")


rm(iati_list, iati_marked, iati_recs_grouped, iati_recs_grouped_all, iati_recs_unique, regionList, summary)

iati = iati_marked_isos

rm(iati_marked_isos)

save(iati, file = "iati_2021_marked_w_iso_region.rds")

save(iati_recs, file = "iati_2021_recs.rds")

rm(list = ls())
gc()
