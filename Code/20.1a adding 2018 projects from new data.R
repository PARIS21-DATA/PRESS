load("Data/Intermediate/06.2 crs and press with region and country code_2023.Rdata")

df_crs_raw <- read_feather("Data/Intermediate/06.3 crs with donor code_2023.feather")


df_crs_2018 <- df_crs_raw %>% 
  select(db_original_id, 
         year, 
         dac_donorname, 
         recipientname, 
         projectnumber, 
         purposecode) %>% 
  anti_join(df_crs) %>% 
  inner_join(df_crs_raw) %>% 
  filter(year == 2018)  %>% 
  # select(projecttitle, usd_disbursement_defl, ch_name) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  filter(projecttitle != "Open Data Watch") #%>% 
  # select(usd_disbursement_defl) %>%
  # sum(na.rm = T)
  # .$projecttitle


# df_crs_raw_may <- read_rds("data/raw/crs/crs_full_2023-03-30.rds")
df_crs_raw_may <- read_rds("data/intermediate/crs01_1_full_2023.rds")
names(df_crs_raw_may)
df_crs_raw_may_sub <-  df_crs_raw_may %>% 
  # select(db_original_id = crsid,
  #        year, 
  #        dac_donorname = donorname, 
  #        recipientname, 
  #        projectnumber, 
  #        purposecode
  #        ,db_ref, 
  #        usd_disbursement_defl,
  #        projecttitle)
  rename(db_original_id = crsid)

df_crs_raw_may_sub <- df_crs_raw_may_sub %>% 
  mutate(year = as.numeric(year))

df_crs_2018_joined <- df_crs_2018 %>% 
  select(db_original_id, purposecode, projectnumber) %>% 
  inner_join(df_crs_raw_may_sub) %>% 
  filter(year ==2018)

df_crs_2018_joined$usd_disbursement_defl %>% sum(na.rm = T)


setdiff(names(df_crs), 
        names(df_crs_raw))

setdiff(names(df_crs), 
        names(df_crs_2018_joined))


df_crs_2018_joined <- df_crs_2018_joined %>% 
  mutate(dac_donorname = donorname, 
         dac_donorcode = donorcode, 
         dac_recipientcode = recipientcode, 
         dac_regioncode = regioncode, 
         dac_regionname = regionname,
         dac_regionncode = regioncode) %>% 
  mutate(commitment_year = substr(commitmentdate, 1, 4), 
         commitment_year = ifelse(is.na(commitment_year), 
                                  year, 
                                  ifelse(commitment_year < 2013,
                                         year, 
                                         commitment_year))) %>% 
  mutate(#donor_type = NA, 
         finance_t_name = NA)

df_donors <- read_feather("data/auxiliary/reporters_crs_2023.feather")

df_crs_2018_joined <- df_donors %>% 
  select(dac_donorcode = crs_code, 
         ch_name, 
         ReporterId = ReporterId_chlist) %>% 
  unique %>% 
  inner_join(df_crs_2018_joined )
  
df_crs_2018_joined <- df_crs_2018_joined %>% 
  mutate(isocode = countrycode(recipientname, "country.name", "iso3c")) 

df_crs_2018_joined <- df_crs_2018_joined %>% 
  mutate(finance_t_name = ifelse(finance_t == 421, "Standard loan", "Standard grant"))

df_crs_2018_joined$bi_multi 

df_crs_2018_joined <- df_crs_2018_joined %>% 
  mutate(donor_type = case_when(bi_multi == 1 ~ "Bilateral", 
                                bi_multi == 4 ~ "Multilateral", 
                                bi_multi == 6 ~ "Private", 
                                TRUE ~"NA"))

df_crs_new <- df_crs %>% 
  plyr::rbind.fill(df_crs_2018_joined)

df_crs_2018_joined %>% 
  filter(is.na(isocode)) %>% 
  select(recipientname) %>% 
  unique

df_crs %>% 
  filter(finance_t_name == "Standard grant") %>% 
  # filter(mining) %>% 
  group_by(year, donor_type) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = donor_type )

df_crs %>% 
  filter(year == 2021) %>% 
  filter(purposecode != 16062) %>% 
  filter(!stats_filter) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
         usd_disbursement_defl, projecttitle, 
         longdescription) %>% 
  slice(1:20)

