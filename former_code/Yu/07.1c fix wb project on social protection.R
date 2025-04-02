# rm(list = ls())
# source("Code/00. boot.R")
# 
# path_input <- paste0("Output/CH/", 
#                      "current", 
#                      " PRESS 2023 data.feather")
# 
# df_crs <- read_feather(path_input)

# path_projects_2remove <- paste0("data/intermediate/07.1b_b social protection projects 2 remove ",
#                                 year(Sys.Date()),
#                                 ".rds")
# vec_projects_2remove <- readRDS(path_projects_2remove)
# 
# df_crs %>% filter(db_ref %in% vec_projects_2remove) %>% 
#   select(projecttitle)

df_crs %>% 
  filter(projecttitle == "Strengthening Systems for Social Protection and Civil Registration Project")


df_crs %>% 
  # filter(gender_filter_both) %>% 
  # filter(gender_filter_both_desc_rmnch) %>% 
  filter(year > 2017) %>%
  # filter(identified_by_gender == "title") %>% 
  filter(grepl("social protection|data disaggregation|intersectional", 
               projecttitle, 
               ignore.case = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl, year) %>% 
  # group_by(projecttitle, year) %>% 
  group_by(projecttitle) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  # group_by(year) %>% 
  filter(row_number() < 6)


var_constant <- (37+50)/323

df_crs_wbSocPro <- df_crs %>% 
  filter(projecttitle == "Strengthening Systems for Social Protection and Civil Registration Project")

df_crs_wbSocPro <- df_crs_wbSocPro %>% 
  mutate(gen_title = T, 
         gender_filter_both = T)  %>% 
  mutate(usd_disbursement_defl = usd_disbursement_defl * var_constant )

df_crs <- df_crs %>% 
  filter(!db_ref %in% df_crs_wbSocPro$db_ref) %>% 
  rbind(df_crs_wbSocPro)

rm(var_constant)
rm(df_crs_wbSocPro)
