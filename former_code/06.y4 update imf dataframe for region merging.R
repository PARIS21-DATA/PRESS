
df_imf <- read_rds("data/Intermediate/99.02b imf.rds")
df_imf <- df_imf %>%
  rename(donorname_survey = donorname)
df_imf %>% write_csv("Data/Intermediate/06.4 intermediate imf.csv")

df_imf_raw <- read_xlsx("data/Intermediate/06.4 intermediate imf_fixed.xlsx")
df_imf_raw %>% write_feather("data/Raw/Survey/imf_2021_2023correction.feather")
