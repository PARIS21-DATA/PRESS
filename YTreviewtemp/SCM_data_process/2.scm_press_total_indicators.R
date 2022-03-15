rm(list = ls())
load("./YTreviewtemp/press2021_from_excel.rds")
press2021_short = press2021 %>%
  filter(countrySpecific ) %>%
  select(ListRecip, usd_commitment, CommitmentDate) %>%
  mutate(ListRecip = ifelse(ListRecip == "CuraÃ§ao", "Curaçao", ListRecip) )%>%
  mutate(iso = countrycode(ListRecip, "country.name","iso3c")) %>%
  mutate(iso = ifelse(ListRecip == "Kosovo", "XKX", iso)) %>%
  filter(!is.na(iso))  %>%
  filter(CommitmentDate <2020) 


press_summary = press2021_short %>%
  filter(!is.na(usd_commitment)) %>% # listrecip contains multiple names for the same country
  group_by(iso,
           # ListRecip, 
           CommitmentDate ) %>%
  summarise(total = sum(usd_commitment, na.rm = T))  %>%
  spread(CommitmentDate, total, fill = 0) %>%
  gather(key = "year", value = "total", -iso
         # , -ListRecip
         ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(y3 = rollsum(total, 3,fill = NA,  align = "right")) %>%
  filter(year == 2019) %>%
  mutate(total = ifelse(total ==0 , NA, total), 
         y3 = ifelse(y3 == 0, NA, y3)) %>%
  filter(!(is.na(total) & is.na(y3))) %>%
  rename(`121`=total, 
         `123`=y3
         # , 
         # country = ListRecip
         ) %>%
  filter(iso !="XKX")


press_summary[is.na(press_summary)] = 0

unique(press_summary$iso) %>%
  length


press_summary = press_summary %>%
  mutate(country = countrycode(iso, "iso3c", "country.name")) %>%
  select(iso, country, year, `121`, `123`)


save(press_summary , file = "./ytreviewtemp/scm_press_total_indicators.rds")

load("./YTreviewtemp/scm_csaindicators.rds")

write_csv(press_summary , file = "./YTreviewtemp/indicator-121-123.csv")
write_csv(press2021_domain_sum_wide, file = "./YTreviewtemp/indicator-105-106-107-108-167.csv")





