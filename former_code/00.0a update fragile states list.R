df_fragile_states <- read_csv("data/auxiliary/fragile states.csv")
df_fragile_states <- df_fragile_states %>% 
  gather(key = "year", value = "fs", na.rm = T, -Countries)  %>% 
  mutate(year = as.numeric(year))
df_fragile_states <- df_fragile_states %>% 
  mutate(isocode = countrycode(Countries, "country.name", "iso3c")) %>% 
  mutate(isocode = ifelse(Countries == "Kosovo", "XKX", isocode)) %>% 
  select(-Countries)
write_feather(df_fragile_states, paste0("data/auxiliary/fragile states ", 
                                        year(Sys.Date()), 
                                        ".feather"))