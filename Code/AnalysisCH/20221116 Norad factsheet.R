rm(list = ls())
source("code/00. boot.R")

df_crs <- readRDS("data/Intermediate/final_PRESS_2022.rds")
names(df_crs)
df_shortnames <- readRDS("data/auxiliary/reporters_shorten_2022.rds")
df_ch_names <- readRDS("data/auxiliary/reporters_types_2022.rds")
df_crs <- df_ch_names %>% 
  rename(ch_name = ReporterName) %>% 
  select(-ReporterType) %>% 
  inner_join((df_shortnames)) %>% 
  select(-ReporterId) %>% 
  right_join(df_crs) %>% 
  mutate(ch_name = ifelse(is.na(short_names),ch_name, short_names))


df_crs <- df_crs %>% 
  mutate(ch_name = ifelse(ch_name == "Bill and Melinda Gates Foundation" , 
                          "Gates Foundation", 
                          ch_name),  
         ch_name = ifelse(ch_name == "The World Bank" , 
                          "World Bank", 
                          ch_name) , 
         ch_name = ifelse(ch_name == "William and Flora Hewlett Foundation" , 
                          "Hewlett Foundation", 
                          ch_name) )


# vec_norad_countries <- df_crs %>% 
#   filter(disbursementdate > 2017, 
#          ch_name == "Norway") %>% 
#   .$isocode %>% 
#   unique 
# 
# vec_norad_countries <- vec_norad_countries[!is.na(vec_norad_countries)]
# 
# df_crs %>% 
#   filter(disbursementdate > 2017, 
#          isocode %in% vec_norad_countries) %>% 
#   select(ch_name, projecttitle, usd_commitment_defl) 



df_norad_keycountries <- df_crs %>% 
  filter(disbursementdate > 2017, 
         !is.na(isocode), 
         ch_name == "Norway") %>% 
  group_by(isocode) %>% 
  summarise(total_norway  =  sum(usd_disbursement_defl, na.rm = T)) %>% 
  filter(total_norway > 100000) %>% 
  arrange(desc(total_norway)) 

df_other_donors <- df_crs %>% 
  filter(disbursementdate > 2017, 
         isocode %in% df_norad_keycountries$isocode, 
         ch_name != "Norway") %>% 
  group_by(ch_name, isocode) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(isocode, desc(total)) %>% 
  group_by(isocode) %>% 
  # mutate(rnk = row_number()) %>% 
  filter(row_number() < 6)



df_matrix_norwayVothers <- df_norad_keycountries %>% 
  mutate(ch_name = "Norway") %>% 
  rename(total = total_norway) %>% 
  rbind(df_other_donors) %>% 
  # right_join(df_norad_keycountries) %>% 
  # arrange(desc(total_norway), desc(total)) %>% 
  # select(-total_norway) %>% 
  # group_by(isocode) %>% 
  # mutate(rnk = row_number(), 
  #        rnk = ifelse(ch_name == "Norway", 0, rnk)) %>% 
  # filter(rnk < 7 ) %>%
  # group_by(isocode) %>% 
  # mutate(n = n()) %>% 
  # filter((n==6 & rnk < 6) | (n==7&  rnk!=6     )) %>% 
  # select(-rnk, -n) %>% 
  spread(value = total, key = isocode) 

df_matrix_norwayVothers <- df_matrix_norwayVothers[c(which(df_matrix_norwayVothers$ch_name == "Norway"), 1:nrow(df_matrix_norwayVothers)),] %>% unique

write_csv(df_matrix_norwayVothers, file = "YTreviewtemp/norad_matrix.csv", na = "")



df_norway_countries_dis <- df_matrix_norwayVothers %>% 
  gather(key = "isocode", value = "disbursements", -ch_name, na.rm = T) %>% 
  arrange(isocode, desc(disbursements)) %>% 
  # select(-disbursements) %>% 
  group_by(isocode) %>% 
  mutate(rnk = row_number())  %>% 
  filter(ch_name == "Norway") %>% 
  rename(Disbursements_by_Norway = disbursements, 
         Norway_ranking = rnk) %>% 
  select(-ch_name)

  

  
df_other_donors %>% 
  group_by(isocode) %>% 
  mutate(rnk = row_number(), 
         rnk = paste0("Top ", rnk)) %>% 
  spread(key = rnk , value = ch_name) %>% 
  write_csv(file = "YTreviewtemp//top5donors_norad.csv")




df_coordination_matrix <- df_other_donors %>% 
  group_by(isocode) %>% 
  mutate(rnk = row_number(), 
         rnk = paste0("Top ", rnk)) %>% 
  left_join(df_norway_countries_dis) %>% 
  ungroup %>% 
  mutate(share_v_norway = total/Disbursements_by_Norway, 
         Disbursements_by_Norway = paste0("$", round(Disbursements_by_Norway/1000, 0), "K"), 
         ch_name = paste0(ch_name, ": ", round(share_v_norway, 1), "X")) %>% 
  select(ch_name, 
         Recipient_Country = isocode, rnk, Disbursements_by_Norway, Norway_ranking) %>% 
  spread(key = rnk, value = ch_name)


write_csv(df_coordination_matrix, file = "YTreviewtemp/norway_coordination_matrix.csv")


# df_other_donors %>% select(ch_name, isocode) %>% inner_join(df_crs) %>% 
#   select(db_ref, isocode, ch_name, usd_disbursement_defl, gender)


df_crs %>% 
  select(db_ref, recipient = isocode, donor = ch_name, usd_disbursement_defl, gender =  gender_filter_both_rmnch, projecttitle, longdescription, 
         commitmentdate, usd_commitment_defl) %>% 
  inner_join(select(df_other_donors,donor = ch_name, recipient = isocode)) %>% 
  ungroup() %>% 
  filter(usd_commitment_defl > 0, 
         # usd_disbursement_defl > 10000,
         commitmentdate > 2018) %>% 
  filter(donor != "IMF") %>% 
  arrange(projecttitle == "", 
          longdescription =="" |is.na(longdescription), 
          # isocode , 
          desc(usd_commitment_defl)) %>% 
  select(-db_ref, -usd_disbursement_defl) %>% 
  rename(`Project title` = projecttitle, 
         Donor = donor, 
         `Long Description` = longdescription, 
         `Commitment Year` = commitmentdate) %>% 
  write_csv("YTreviewtemp/norway_projects_by_partners.csv", na = "")


