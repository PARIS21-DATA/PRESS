rm(list = ls())
# load("./data/raw/survey/imf_2021_with_region.rds")
# write_rds(imf, file = "data/Intermediate/99.02b imf.rds")

# load("./analysis/press2021_v3.rds")
df_imf <- read_rds("data/Intermediate/99.02b imf.rds")
df_press <- read_rds("./data/Intermediate/99.02 press_crs_merged_reduced.Rds")
names(df_imf) %in% names(df_press)

names(df_imf)[!names(df_imf) %in% names(df_press)]

# press$disbursement %>% tail
# imf$commitmentdate = imf$commitmentdate-1
# names(imf) %in% names(press)
# press$source %>% unique()
# imf$commitmentdate %>% unique
# is.numeric(press$usd_disbursement)
# is.numeric(imf$usd_disbursement)
df_press %>% filter(donorname == "International Monetary Fund - IMF") %>% select(commitmentdate) %>%
  unique()

df_imf <- df_imf %>% 
  mutate(ch_name = "International Monetary Fund", 
         ReporterId = 907)

df_press = plyr::rbind.fill(df_press, df_imf)

sum(df_press$usd_disbursement, na.rm = T)
sum(df_press$usd_commitment, na.rm = T)

write_rds(df_press , file = "./data/intermediate/99.03 crs_press_imf_2021.rds")
