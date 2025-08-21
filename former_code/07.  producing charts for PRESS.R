
df_merged <- read_rds("data/intermediate/final_PRESS_2022.rds")

# figure 1
df_merged %>% 
  group_by(disbursementdate) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  write_csv("output/2022disb.csv")

# figure 2
# create the line for dac
df_merged %>% 
  filter(source == "crs") %>% 
  filter(ReporterType == "DAC") %>%
  group_by(disbursementdate) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) 

# figure 3 see the end. needs to clear env before running. So do it after all is done 

# figure 4
df_merged %>% 
  filter(gender_filter_both_rmnch) %>% 
  group_by(disbursementdate) %>%  
  filter(disbursementdate > 2010) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) 



# figure 5
df_merged %>% 
  filter(gender_filter_both_rmnch) %>% 
  filter(identified_by_gender!= "Gender equality marker") %>%
  group_by(ch_name) %>% 
  filter(disbursementdate > 2015) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  mutate(sum = sum(total), 
         share = total/sum) 


# figure 6 
df_merged %>% 
  filter(commitmentdate > 2010) %>% 
  group_by(commitmentdate) %>% 
  summarise(total = sum(usd_commitment_defl, na.rm = T)) %>% 
  write_csv("output/2022commit.csv") 



# figure a.1

df_merged %>% 
  filter(gender_filter_both_rmnch) %>% 
  group_by(ReporterType, disbursementdate) %>%  
  filter(disbursementdate > 2010) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = ReporterType, value = total, fill = 0) %>% 
  mutate(sum = DAC + multilateral + `non-DAC`+ private) %>% 
  write_csv("output/2022gendrdonortrend.csv") 


# figure a.2 
df_country_allocation <- df_merged %>% 
  filter(!is.na(isocode)) %>% 
  filter(disbursementdate > 2010) %>% 
  group_by(disbursementdate, isocode) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(disbursementdate, desc(total)) %>% 
  group_by(disbursementdate) %>% 
  mutate(year_total = sum(total), 
         share  = total/year_total)  %>% 
  filter(!is.na(isocode)) %>% 
  group_by(disbursementdate) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank < 26) %>% 
  group_by(disbursementdate) %>%
  mutate(share_top25 = sum(share))

df_country_allocation %>% 
  select(disbursementdate, share, rank) %>% 
  spread(key = disbursementdate, value = share) %>% 
  filter(rank < 6) %>% 
  arrange(desc(rank)) %>% 
  write_csv( file = "output/2022top5recipients.csv")

df_country_allocation %>% 
  select(disbursementdate, share_top25) %>% 
  unique %>% 
  spread(key = disbursementdate, value = share_top25) %>% 
  write_csv( file = "output/2022top25recipients.csv")


# figure a.3 
df_fs <- read_csv("data/auxiliary/fragile states.csv") 

df_fs <- df_fs %>% 
  gather( key = "disbursementdate", value = "fs", -Countries) %>% 
  mutate(disbursementdate = as.numeric(disbursementdate)) %>% 
  filter(disbursementdate > 2017, fs ==1) %>% 
  mutate(isocode = countrycode(Countries, "country.name", "iso3c")) %>% 
  mutate(isocode = ifelse(is.na(isocode), "XKX", isocode)) %>%  # for Kosovo
  select(disbursementdate, isocode)

df_merged %>% 
  inner_join(df_fs) %>% 
  group_by(isocode) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  mutate(country = countrycode(isocode, "iso3c","country.name")) 


# figure a.4
df_sids <- read_csv("data/auxiliary/sids.csv")
df_merged %>% 
  inner_join(df_sids) %>% 
  filter(disbursementdate > 2017) %>% 
  group_by(isocode) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  mutate(country = countrycode(isocode, "iso3c","country.name")) %>% 
  write_csv("output/2022sids.csv")

# figure a.5
df_merged %>% 
  group_by(ch_name) %>%  
  filter(disbursementdate > 2017) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  write_csv("output/2022donorrankings.csv")



# figure 3
source("code/95.01 gender equality chart.R")



