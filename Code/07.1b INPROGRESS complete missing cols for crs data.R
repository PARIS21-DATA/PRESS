source("code/06.2 add unified rec and region code to CRS.R")
source("code/06.3 add donor codes to crs and survey.R")
rm(list = ls())
source("Code/00. boot.R")
time_start <- Sys.time()
path_crs_4ref <- "data/Intermediate/06.3 crs with donor code_2023.feather"

path_crs_output_constant <- paste0("Output/CH/", 
                          "current", 
                          " PRESS 2023 data.feather")

path_crs_output <- paste0("Output/CH/", 
                          Sys.Date(), 
                          " PRESS 2023 data.feather")

path_crs_output_RDS <- paste0("Output/CH/", 
                              Sys.Date(), 
                              " PRESS 2023 data.RDS")

df_crs <- read_feather(path_crs_4ref)

df_crs <- df_crs %>% 
  mutate(year = as.numeric(year))

df_crs <- df_crs %>% 
  filter(purposecode != 15250)

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



df_crs <- df_crs  %>% 
  mutate(identified_by_stat = ifelse(scb, 
                                    "PP", 
                                    ifelse(text_detection, 
                                           "title", 
                                           ifelse(stats_filter_desc, 
                                                  "desc", 
                                                  ifelse(d4d_addition_search|d4d_addition_match, 
                                                         "d4d", 
                                                         "something else")))))
df_crs$identified_by_stat %>% table


df_crs <- df_crs %>% 
  mutate(infosys = grepl("infosys|info sys|information sys|système d'information|system info", projecttitle, ignore.case = T))

df_crs <- df_crs %>% 
  rename(dac_donorcode = donorcode_dac, 
         dac_donorname = donorname_dac)


df_crs <- df_crs %>% 
  rename(gen_desc = gender_filter_desc, 
         gen_title = text_detection_gender)


df_crs <- df_crs %>% 
  mutate(donor_type = ifelse(bi_multi == 4, 
                             "Multilateral", 
                             ifelse(bi_multi == 6, 
                                    "Private", 
                                    "Bilateral")))

df_crs$finance_t %>% table
df_financing_type <- tibble(finance_t = c(110, 421, 520), 
                            finance_t_name = c("Standard grant",
                                               "Standard loan", 
                                               "Shares in collective investment vehicles "))

df_crs <- df_crs %>% 
  inner_join(df_financing_type) 
rm(df_financing_type)

source("code/07.1e remove some mistakes in the rmnch marking.R")
# source("code/07.1c fix wb project on social protection.R")
source("code/07.1d adding more gender filters and create identified by column.R")
source("code/07.1b_c filter for innovation.R")

write_feather(df_crs, path_crs_output_constant)
write_feather(df_crs,path_crs_output)
saveRDS(df_crs, path_crs_output_RDS)
print_time_diff(time_start)

# df_crs_old <- read_feather("output/ch/2023-09-13 PRESS 2023 data.feather")
# df_crs_old %>% 
#   group_by(year) %>% 
#   summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_crs %>% 
  # filter(identified_by_stat =="d4d") %>%
  # filter(d4d_addition_search) %>%
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_crs$d4d_addition_search %>% table


# df_d4d_searched <- df_crs %>% 
#   filter(d4d_addition_search) %>%
#   filter(year > 2018) %>%
#   arrange(desc(usd_disbursement_defl)) %>%
#   select(projecttitle, usd_disbursement_defl, 
#          db_original_id, 
#          projectnumber, 
#          d4d_addition_search)



