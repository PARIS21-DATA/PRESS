
rm(list = ls())
press2021 = read_xlsx("./YTreviewtemp/PRESS 202112.xlsx")

load("./YTreviewtemp/csa.rds")

head(csa)
nchar("predictions_domain3")
csa_long = csa %>%
  gather(domain, value, -db_ref) %>%
  mutate(domain = substr(domain,19, 19 ), 
         domain = as.numeric(domain)) %>%
  filter(value == 1)  %>%
  select(-value)

csa_number = csa_long %>%
  group_by(db_ref) %>%
  summarise(n_of_domain  = n()) 
  
press2021_csa = inner_join(press2021, csa_number) %>%
  filter(!is.na(usd_commitment)) %>%
  mutate(usd_commitment_per_domain = usd_commitment/n_of_domain) %>%
  select(-n_of_domain) %>%
  inner_join(csa_long)

press2021_csa_iso = press2021_csa %>%
  filter(countrySpecific ) %>%
  select(ListRecip, usd_commitment, CommitmentDate, usd_commitment_per_domain, 
         domain) %>%
  mutate(iso = countrycode(ListRecip, "country.name", "iso3c")) %>%
  mutate(iso = ifelse(ListRecip == "Kosovo", "XKX", iso)) %>%
  filter(!is.na(iso))  %>%
  filter(CommitmentDate <2020)


press2021_csa_iso_domain_sum  = press2021_csa_iso %>%
  group_by(iso, CommitmentDate, domain) %>%
  summarise(domain_sum = sum(usd_commitment_per_domain)) %>% 
  arrange(iso, domain, CommitmentDate) %>%
  spread(domain, domain_sum, fill = 0) %>%
  gather(key = domain, value = domain_sum, -iso, -CommitmentDate) %>%
  spread(CommitmentDate, domain_sum, fill = 0) %>%
  gather(key = CommitmentDate, value = domain_sum, -iso, -domain)

press2021_10year_sum = press2021_csa_iso_domain_sum  %>%
  group_by(iso, domain) %>%
  summarise(y10sum = sum(domain_sum))  %>%
  group_by(iso) %>%
  mutate(overall = sum(y10sum, na.rm = T), 
         year = 2019, 
         share = y10sum/overall) %>%
  select(iso, domain, year, share)

press2021_9year_sum= press2021_csa_iso_domain_sum  %>%
  group_by(iso, domain) %>%
  arrange(iso, domain, CommitmentDate) %>%
  mutate(y9sum = rollsum(domain_sum, 9, fill = NA, align = "right")) %>%
  group_by(iso) %>%
  filter(CommitmentDate == 2018) %>%
  mutate(overall = sum(y9sum, na.rm = T), 
         year = 2018, 
         share = y9sum/overall)  %>%
  select(iso, domain, year, share)

press2021_8year_sum= press2021_csa_iso_domain_sum  %>%
  group_by(iso, domain) %>%
  arrange(iso, domain, CommitmentDate) %>%
  mutate(y9sum = rollsum(domain_sum, 8, fill = NA, align = "right")) %>%
  group_by(iso) %>%
  filter(CommitmentDate == 2017) %>%
  mutate(overall = sum(y9sum, na.rm = T), 
         year = 2017, 
         share = y9sum/overall)  %>%
  select(iso, domain, year, share)

press2021_7year_sum= press2021_csa_iso_domain_sum  %>%
  group_by(iso, domain) %>%
  arrange(iso, domain, CommitmentDate) %>%
  mutate(y9sum = rollsum(domain_sum, 7, fill = NA, align = "right")) %>%
  group_by(iso) %>%
  filter(CommitmentDate == 2016) %>%
  mutate(overall = sum(y9sum, na.rm = T), 
         year = 2016, 
         share = y9sum/overall)  %>%
  select(iso, domain, year, share)


press2021_domain_sum = rbind(press2021_10year_sum, press2021_7year_sum, press2021_8year_sum, press2021_9year_sum)
rm(press2021_10year_sum, press2021_7year_sum, press2021_8year_sum, press2021_9year_sum)

domains = data.frame(domain = as.character(1:5), 
                     domain_names =c("demographic and social statistics", 
                                     "economic statistics", 
                                     "environmental and multi-sectoral statistics", 
                                     "general data collection, preservation and dissemination", 
                                     "strategic and managerial issues of official statistics"), 
                     indicator_n = c(105:108, 167))


press2021_domain_sum = press2021_domain_sum %>%
  inner_join(domains) %>%
  select(-domain, -domain_names)


press2021_domain_sum_wide = press2021_domain_sum %>%
  spread(indicator_n, share) %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"), 
         country = ifelse(is.na(country), "Kosovo", country)) %>%
  filter(country !="Kosovo")

press2021_domain_sum_wide = press2021_domain_sum_wide %>%
  select(iso, country, year, `105`, `106`, `107`, `108`, `167`)


save(press2021_domain_sum_wide, file = "./YTreviewtemp/scm_csaindicators.rds")

