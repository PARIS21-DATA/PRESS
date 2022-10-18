
# setwd("~/Dropbox/PARIS21/PRESS/2021/")
rm(list = ls())
df_press <- read_rds("./data/Intermediate/99.01 merged_press_crs_2021_v2.Rds")
# merged_press_CRS %>% select(donorname, commitmentdate, source) %>%
#   filter(donorname == "Asian Development Bank") %>% 
#   unique

# df_crs %>% filter(commitmentdate> 2010) %>% group_by(commitmentdate) %>% dplyr::summarise(total = sum(usd_commitment, na.rm = T)) 




# iati_posi = iati %>%
#   filter(usd_commitment >0 )
# 
# all_posi = all_merged %>%
#   filter(usd_commitment >0 )


press.unique.code.name = df_press %>%
  group_by(donorname) %>%
  dplyr::summarise(donorcode = min(donorcode))

df_press = df_press %>% 
  select(-donorcode) %>%
  left_join(press.unique.code.name)

press_too_early = df_press %>%
  filter(commitmentdate < 2010)



df_press = df_press %>% 
  filter(!(db_ref %in% press_too_early$db_ref))

# save(press_too_early, file = "./analysis/press_crs_merged_tooearly_v2.rds")


# load("./analysis/press_crs_merged_tooearly_v2.rds")

overview = df_press %>%
  filter(usd_commitment > 0) %>%
  group_by(commitmentdate, donorname, source) %>%
  dplyr::summarise(amount = sum(usd_commitment,na.rm = T),
                   n_of_proj = n())


overview2 = overview %>%
  # filter(usd_commitment > 0) %>%
  group_by(commitmentdate, donorname) %>%
  dplyr::summarise(sources = n())   %>%
  filter(sources>1)
# 
# write.csv(overview2, file = "./data/analysis/overview_crs_or_press.csv", row.names = F)

press_full = df_press

overview_fixed = read.csv("./data/analysis/overview_crs_or_press_2022_fixed.csv", stringsAsFactors = F) # taking the conservative path


overview_fixed = overview_fixed %>%
  select(-sources) %>%
  filter(selection != "both") %>%
  mutate(source = ifelse(selection == "press", "CRS", "survey")) %>%
  select( -selection) 


press_full %>% group_by(commitmentdate) %>% dplyr::summarise(total = sum(usd_commitment_defl, na.rm = T), sum(usd_disbursement, na.rm = T)) 

# names(overview_fixed)
# press$source %>% table
press_to_drop = press_full %>%
  inner_join(overview_fixed, by = c("donorname", "commitmentdate","source"))



press = press_full %>%
  filter(!(db_ref %in% press_to_drop$db_ref) )


press %>%
  filter(usd_commitment > 0) %>%
  group_by(commitmentdate, donorname, source) %>%
  dplyr::summarise(amount = sum(usd_commitment,na.rm = T),
                   n_of_proj = n()) %>%
  # filter(usd_commitment > 0) %>%
  group_by(commitmentdate, donorname) %>%
  dplyr::summarise(sources = n())   %>%
  filter(sources>1)


press %>% group_by(commitmentdate) %>% dplyr::summarise(total = sum(usd_commitment, na.rm = T)) 


press %>% filter(commitmentdate > 2016) %>% group_by(donorname) %>% dplyr::summarise(total = sum(usd_commitment, na.rm = T))  %>% arrange(desc(total))

# press_questionables = press %>% 
#   filter(projecttitle == "TC AGGREGATED ACTIVITIES" | objectives == "TC AGGREGATED ACTIVITIES")



press %>% group_by(year, source) %>% dplyr::summarise(total = sum(usd_disbursement, na.rm = T)) %>%
  tidyr::spread(source, total)

df_press %>% group_by(year, source) %>% dplyr::summarise(total = sum(usd_disbursement, na.rm = T)) %>%
  tidyr::spread(source, total)






# press %>% filter(donorname!="The World Bank") %>%
#   group_by(commitmentdate, source) %>% dplyr::summarise(total = sum(usd_commitment, na.rm = T)) %>%
#   tidyr::spread(source, total)

rm( overview_fixed)

# press$picked_up_by %>% table

write_rds(press, file = "./data/intermediate/99.02 press_crs_merged_reduced.Rds")

# save(press_to_drop, press_full, file = "./analysis/press_leftover_afterCRSmerging_removeduplicatedReports.rdata")
# rm(list = ls())
# 
# press_to_examine = press %>% 
#   filter(commitmentdate %in% c(2011:2013)) %>%
#   arrange(commitmentdate, desc(usd_commitment)) %>%
#   select(donorname, donorcode, projecttitle, objectives,commitmentdate, shortdescription, source, db_ref, usd_commitment) %>%
#   filter(usd_commitment > 20000000)



# unique(press$donorname)

# 
# overview = press %>%
#   filter(usd_commitment > 0) %>%
#   group_by(commitmentdate, donorcode, source) %>%
#   dplyr::summarise(amount = sum(usd_commitment,na.rm = T), 
#                    n_of_proj = n()) 

