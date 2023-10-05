rm(list = ls())

# 0. load data
df_crs <- read_feather("output/CH/current PRESS 2023 data.feather")
df_crs_raw <- read_feather("data/intermediate/crs05.2_full_2023.feather")

# 0.1 basic tabulation
df_crs %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) 

# 0.2 prepare a new env for chart data
e_fig <- new.env()

# 0.3 tmp adding variables to crs_raw
df_crs_raw <- df_crs_raw %>% 
  mutate(donor_type = ifelse(bi_multi == 4, 
                             "Multilateral", 
                             ifelse(bi_multi == 6, 
                                    "Private", 
                                    "Bilateral")))

write_feather()


df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  fun_summarise_ts(group_var = "finance_t") %>% 
  mutate(sum = `110` + `421`)


df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  select(dac_donorcode) %>% 
  distinct




df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  filter(identified_by_stat != "ppcode") %>% 
  filter(purposecode != 16062) %>% 
  group_by(projecttitle, projectnumber, recipientname) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, recipientname, total) %>%
  ungroup %>% 
  slice(1:20) %>% 
  write.xlsx("output/press/20230921 discussion with WB - data projects.xlsx")


df_wb_raw <- df_crs_raw %>% 
  # filter(ch_name != "The World Bank") %>% 
  filter(donorcode == 905 | donorcode == 901) %>% 
  filter(year > 2017) %>% 
  filter(!db_ref %in% df_crs$db_ref) 

# nothing
df_wb_raw %>% 
  filter(grepl("data|statis", longdescription, T)) %>%
  arrange(desc(usd_disbursement_defl)) %>% 
  select(projecttitle, usd_disbursement_defl) 


# nothing
df_wb_raw %>% 
  filter(grepl("information", projecttitle, T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total)

df_wb_raw %>% 
  filter(grepl("information", projecttitle, T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total) %>% 
  write.xlsx("output/press/20230921 discussion with WB - non data projects.xlsx")


df_wb_raw %>% 
  filter(grepl("digitali", projecttitle, T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total)

df_wb_raw %>% 
  filter(grepl("digitali", projecttitle, T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total)%>% 
  write.xlsx("output/press/20230921 discussion with WB - non data projects 2.xlsx")


e_fig$top_sector_projects <- df_wb_raw %>% 
  filter(grepl("health|education|epidem|social prot|regist", projecttitle, T)) %>%
  # filter(grepl("data|statist|census|survey",
  #              longdescription,
  #              T)) %>%
  group_by(projecttitle
           # , projectnumber
           ,recipientname
           ) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle,
         # projectnumber, 
         recipientname,
         total) %>% 
  filter(total > 150) %>% 
  left_join(df_wb_raw %>% select(projecttitle, projectnumber) ) %>%
  mutate(projectnumber = gsub(pattern = "\\.crs\\d+$", 
                              "", 
                              projectnumber)) %>% 
  distinct %>% 
  group_by(projecttitle, 
           recipientname, 
           total) %>% 
  summarise(projectnumbers = paste(projectnumber, 
                                   collapse = "; "))



e_fig$top_sector_projects$projectnumbers

e_fig$top_sector_projects  %>% 
  write.xlsx("output/press/20230921 discussion with WB - sectoral projects.xlsx")


df_wb_raw %>% 
  filter(grepl("analy", projecttitle, T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total)

df_wb_raw %>% 
  filter(grepl("eviden", projecttitle, ignore.case = T)) %>%
  group_by(projecttitle, projectnumber) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(desc(total)) %>% 
  select(projecttitle, projectnumber, total)


df_wb_raw %>% 
  group_by(year) %>% 
  summarise(total  = sum(usd_disbursement_defl, na.rm = T)) 
df_wb_raw %>% 
  fun_summarise_ts("donorcode")
