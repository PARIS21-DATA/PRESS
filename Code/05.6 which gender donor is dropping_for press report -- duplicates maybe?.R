df_crs %>% group_by(commitmentyear
                    # , text_filter_gender
) %>% 
  filter(commitmentyear > 2010) %>% 
  filter(stats_wo_infoSys) %>% 
  # filter(year > 2016) %>% 
  # filter(gender_filter_both_rmnch) %>%
  summarise(#total = sum(usd_disbursement_defl), 
    commit = sum(usd_commitment_defl),
    cnt = n()) 


df_crs %>% 
  filter(stats_wo_infoSys) %>% 
  filter(commitmentyear==2020 ) %>% 
  arrange(desc(usd_commitment_defl)) %>% 
  select(projecttitle, usd_commitment_defl, donorname, crsid, longdescription, expectedstartdate, purposecode, identified_by) %>% 
  slice(1:10)

# what we have excluded by excluding infosys? 
df_crs %>% 
  filter(!stats_wo_infoSys) %>% 
  group_by(year) %>% 
  summarise(#total = sum(usd_disbursement_defl), 
    commit = sum(usd_disbursement_defl),
    cnt = n()) 

# feels reasonable to exclude them
df_crs %>% 
  filter(!stats_wo_infoSys) %>% 
  filter(commitmentyear==2020 ) %>% 
  arrange(desc(usd_commitment_defl)) %>% 
  select(projecttitle, usd_commitment_defl, donorname, crsid, longdescription, expectedstartdate, purposecode, identified_by) %>% 
  slice(1:10)

# projecttitle
# 1                                                                         Global programme _ Division of Information Systems and Telecoms
# 2  Strengthening Health Management Information Systems, Laboratory Network and Procurement and Supply Chain Management Systems in Nigeria
# 3                                                                    Development of CMC(Colombo Municipal Council) Tax Information System
# 4                                                Enhancing Climate Information Systems for Resilient Development in Liberia (Liberia CIS)
# 5                                                Enhancing Climate Information Systems for Resilient Development in Liberia (Liberia CIS)
# 6                                                                       HealthIT: Sustaining Use of District Health Information System II
# 7                                                                                   Eswatini Client Management Information System (ECMIS)
# 8       AC_679_4.1.1 Countries enabled to strengthen data, analytics and health information systems to inform policy and deliver impacts.
# 9                                                                   Open source Health Insurance Management Information System (OpenIMIS)
# 10      AC_789_4.1.1 Countries enabled to strengthen data, analytics and health information systems to inform policy and deliver impacts.



df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(stats_wo_infoSys) %>% 
  filter(year > 2017) %>%
  # filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl)
            # commit = sum(usd_commitment_defl),
  ) %>% 
  # filter(total > 10) %>% 
  spread(key = year, value = total, fill = 0) %>% 
  mutate(change_2018 = `2020`/`2018`, 
         change_2019 = `2020`/`2019`, 
         sum = `2018`+`2019`+`2020`) %>% 
  filter(sum > 10) %>% 
  arrange(change_2018)


# FAO (did not report), UNICEF, Italy, USA, Sweden, UK, Gates, Bloomberg, Australis, ILO

df_crs %>% 
  filter(donorname == "Food and Agriculture Organisation") %>% 
  filter(gender_filter_both_rmnch) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl)
            # commit = sum(usd_commitment_defl),
  )



# are we double countring 2018?
df_crs %>% 
  group_by(year) %>% 
  summarise(cnt = n())

df_crs %>% 
  select(year, crsid) %>% 
  unique() %>% 
  group_by(year) %>% 
  summarise(cnt = n())

df_crs %>% 
  filter(year == 2018) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(projecttitle) %>% 
  head(10)

# ok we are fine

df_crs %>% 
  group_by(year, donorname) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank < 10, year  > 2017) %>% 
  filter(donorname %in% c("United States")) %>% 
  ungroup %>%
  arrange(year, rank) %>% 
  select(projecttitle, usd_disbursement_defl , year, rank ) %>% 
  data.frame()

df_crs %>% 
  filter(year > 2017) %>% 
  group_by(year, donorname) %>% 
  summarise(cnt = n()) %>% 
  # filter(total > 10) %>% 
  spread(key = year, value = cnt, fill = 0) %>% 
  mutate(change_2018 = `2020`/`2018`, 
         change_2019 = `2020`/`2019`, 
         sum = `2018`+`2019`+`2020`) %>% 
  filter(sum > 50) %>% 
  arrange(change_2018)


df_crs %>% 
  filter(donorname == "Food and Agriculture Organisation") %>% 
  select(donorcode) %>% 
  unique

df_crs %>% 
  filter(donorcode == 932) %>% 
  group_by(year) %>% 
  summarise(cnt = n())
