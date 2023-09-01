rm(list = ls())
source("code/00. boot.R")
path_input_crs2023 <- "Output/CH/202308 PRESS 2023 data.feather"
path_input_d4d2022 <- "data/D4D/D4D Profiles data_2022_c.xlsx"
path_output <- "output/ch/202308 PRESS 2023 Canada.xlsx"

df_crs <- read_feather(path_input_crs2023)
df_d4d <- read_xlsx(path_input_d4d2022)


df_crs %>% 
  filter(ch_name == "Canada") %>%
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

# need to use the previous year's data and use the new data for this year


df_d4d %>% 
  group_by(Year)  %>% 
  summarise(total_commit = sum(USD_Commitment_Defl, na.rm = T), 
            total_disb = sum(USD_Disbursement_Defl, na.rm = T))

names(df_d4d)
names(df_crs)

vec_names_d4d_lower <- names(df_d4d) %>% tolower
vec_names_crs <- names(df_crs)

setdiff(vec_names_crs, vec_names_d4d_lower)
setdiff(vec_names_d4d_lower, vec_names_crs)

df_crs_4merge <- df_crs %>% 
  rename(donorname = donorname_dac, 
         recipientcode = dac_recipientcode, 
         region = dac_regionname, 
         crsid = db_original_id) %>% 
  filter(year == 2021)


df_d4d %>% filter(DonorName == "Canada") %>% select(DonorCode ) %>% unique

df_crs_4merge_canada <-  df_crs_4merge %>% 
  filter(donorname == "Canada") %>% 
  mutate(donorcode = 301)

df_d4d_4merge_canada <- df_d4d %>% 
  filter(DonorName == "Canada")


vec_names_crs_2select <- intersect(vec_names_crs, vec_names_d4d_lower)
df_crs_4merge_canada <- df_crs_4merge_canada %>% 
  select(all_of(vec_names_crs_2select))

df_crs_rename <- tibble(upper = names(df_d4d), 
                        lower = vec_names_d4d_lower)  %>% 
  inner_join(tibble(lower = vec_names_crs_2select, 
                    order = 1:length(vec_names_crs_2select))) %>% 
  arrange(order)

names(df_crs_4merge_canada) <- df_crs_rename$upper


df_canada_final <- df_d4d_4merge_canada %>% 
  select(all_of(df_crs_rename$upper)) %>% 
  rbind(df_crs_4merge_canada)


df_canada_final %>% 
  write.xlsx(file = path_output)


df_canada_final %>% 
  group_by(Year) %>% 
  summarise(total_commit = sum(USD_Commitment_Defl, na.rm = T), 
            total_disb = sum(USD_Disbursement_Defl, na.rm = T))


