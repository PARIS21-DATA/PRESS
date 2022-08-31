source("Code/00. boot.R")

start <- Sys.time()
df_crs <- read_rds("data/intermediate/crs05.2.1_utf8_full.rds")
print_time_diff(start)
gc()


df_crs_stats <- df_crs %>% 
  filter(stats)

df_crs_o <- df_crs

write_rds(df_crs_stats, "data/intermediate/crs05.3_onlystats_utf8_full.rds")
rm(df_crs)
gc()

df_crs_stats <- read_rds("data/intermediate/crs05.3_onlystats_utf8_full.rds")
df_crs_stats$rmnch %>% unique()

df_crs_stats <- df_crs_stats %>% 
  mutate(usd_disbursement_defl = replace_na(usd_disbursement_defl, 0),  
         usd_commitment_defl = replace_na(usd_commitment_defl, 0)) %>% 
  mutate(gen_rmnch = replace_na(rmnch, 0), 
         gen_rmnch_narrow = gen_rmnch %in% c(3,4), 
         gen_rmnch_broader = gen_rmnch %in% c(2:4)) %>% 
  mutate(gender_filter_both_rmnch = gender_filter_both|gen_rmnch_narrow)

write_rds(df_crs_stats, "data/intermediate/crs05.3_onlystats_utf8_full.rds")

df_crs <- df_crs_stats



# Funding to gender data
# FULL filter ex rmnch
df_crs %>% group_by(year
                    # , text_filter_gender
                    ) %>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl), 
            cnt = n()) 

# check donornames
df_crs %>% arrange(donorname) %>% select(donorname) %>%  unique()

# UK and sweden's contribution
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total_disb = sum(usd_disbursement_defl), 
            total_commit = sum(usd_commitment_defl), 
            cnt = n()) 

df_crs$gen_rmnch_narrow %>% table


