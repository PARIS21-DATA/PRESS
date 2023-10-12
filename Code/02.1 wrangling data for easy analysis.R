### ---------------
# 0. start loading data an set parameters
###
rm(list = ls())
source("Code/00. boot.R")
gc()
Sys.sleep(10)

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
# job_specific_suffix <- ""
job_specific_suffix <- "_full_"
path_input_crs <- paste0("./Data/intermediate/crs01_1", job_specific_suffix,year(Sys.Date()),  ".feather")
# path_intermediate_crs_after_cleaning <- paste0("./Data/intermediate/crs02_int_clean_titles", job_specific_suffix,year(Sys.Date()),  ".feather")
path_output <- paste0("./Data/intermediate/crs02.1 data wrangling", job_specific_suffix,year(Sys.Date()), ".feather")

path_finance_type <- "data/auxiliary/finance types 2023.xlsx"
start <- Sys.time()


## load the data file
print("Loading document:")
# df_crs_raw <- df_crs
# rm(df_crs)
df_crs_raw <- read_feather(path_input_crs)
print_time_diff(start)
df_finance_t <- read_xlsx(path_finance_type)


### ---------------
# 1. select the required columns
###

df_crs <- df_crs_raw %>% 
  select(process_id
         , year
         , finance_t
         , bi_multi
         , commitmentdate) 
  


### ---------------
# 2. add markers and data wrangling for analysis
###

### ---------------
# 2.1 finance type
###

df_finance_t <- df_finance_t %>% 
  rename(finance_t = sub_category_code, 
         finance_t_father_code = category_code, 
         finance_t_name_original = sub_category, 
         finance_t_name_father = category)

df_crs <- df_crs%>% 
  left_join(df_finance_t)
rm(df_finance_t)

### ---------------
# 2.2 donor_type
###

df_crs <- df_crs %>% 
  mutate(donor_type = ifelse(bi_multi == 4, 
                             "Multilateral", 
                             ifelse(bi_multi == 6, 
                                    "Private", 
                                    "Bilateral")))

### ---------------
# 2.3 numeric years
###
df_crs <- df_crs %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

### ---------------
# 2.4 commitment years
###

df_crs_commitment_year <- df_crs %>% 
  ungroup %>% 
  select(commitmentdate, year) %>%
  unique %>% 
  mutate(commitment_year =  substr(commitmentdate, 1, 4))  %>% 
  mutate(commitment_year = as.numeric(commitment_year)) %>% 
  mutate(commitment_year = ifelse(is.na(commitment_year), year, commitment_year)) %>% 
  unique

df_crs <- df_crs %>% 
  inner_join(df_crs_commitment_year)

rm(df_crs_commitment_year)

### ---------------
# 2.5 finance type simplified names
###

df_financing_type <- tibble(finance_t = c(110, 421, 520), 
                            finance_t_name_simplified = c("Standard grant",
                                               "Standard loan", 
                                               "Shares in collective investment vehicles ")) %>% 
  mutate(finance_t_name = finance_t_name_simplified)

df_crs <- df_crs %>% 
  inner_join(df_financing_type) 
rm(df_financing_type)




### ---------------
# 3 save 
###


write_feather(df_crs, path_output)
print_time_diff(start)

