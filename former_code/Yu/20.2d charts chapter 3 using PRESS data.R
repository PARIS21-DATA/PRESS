
# df_names_change <- read_xlsx("data/auxiliary/new_names_for_output.xlsx")
x <- x %>% 
  mutate(country_specific = countrySpecific)
# fun_name_change_4output <- function(x, new_names = df_names_change) {
#   x <- left_join(x, new_names %>% rename(ch_name = original)) %>% 
#     mutate(ch_name = ifelse(is.na(new), 
#                             ch_name, 
#                             new)) 
#   return(x)
# }

source("code/00.1d function_ordinalize.R")
# 1. top recipients
## 1.1 recipients of grants
e_fig$df_crs_recipients <- x %>% 
  filter(countrySpecific) %>% 
  filter(finance_t_name == "Standard grant") %>%
  group_by(year, isocode) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(year, desc(total)) %>% 
  group_by(year) %>% 
  mutate(year_sum = sum(total), 
         top5 = sum(head(total, 5)),
         top25 = sum(head(total, 20))) %>% 
  mutate(top5_share = top5/year_sum, 
         top25_share = top25/year_sum) %>% 
  # select(#-year_sum, 
         # -top5, -top25) %>% 
  filter(row_number()<6) %>% 
  mutate(rank = row_number()) %>% 
  mutate(share = total/year_sum) %>% 
  mutate(rank_text = fun_ordinalize(rank))

# df_crs_recipients %>% 
#   select(-isocode) %>% 
#   spread(key = rank, value = total) 
e_fig$df_crs_recipients_names <- e_fig$df_crs_recipients %>% 
  select(year, isocode, rank_text) %>% 
  spread(key = rank_text, 
         value = isocode)

e_fig$df_crs_recipients
e_fig$df_crs_recipients_names

e_fig$df_crs_recipients_names %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 3.1a top 5 recipients names.xlsx")


e_fig$df_crs_recipients_top5_wide <- e_fig$df_crs_recipients %>% 
  select(year, 
         total
         # ,rank
         ,rank_text) %>% 
  spread(key = rank_text, value = total)


e_fig$df_crs_recipients_top25 <- e_fig$df_crs_recipients %>% 
  select(year, top25_share) %>% 
  unique

e_fig$df_crs_top_recipients_values <- inner_join(e_fig$df_crs_recipients_top5_wide,
                                                 e_fig$df_crs_recipients_top25) %>% 
  rename("% of total received 
by top 25 recipients" = top25_share)

e_fig$df_crs_top_recipients_values 


e_fig$df_crs_recipients_top5_wide %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 3.1 top 5 and 25 recipients values.xlsx")

## 1.2 recipients of loans
# e_fig$top_rec_loans <- df_crs %>% 
#   filter(country_specific) %>% 
#   filter(finance_t_name == "Standard loan") %>% 
#   group_by(year, recipientname) %>% 
#   summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
#   filter(total > 0) %>%
#   arrange(year, desc(total)) %>% 
#   group_by(year) %>% 
#   summarise(year_sum = sum(total), 
#             top5 = sum(head(total, 5)), 
#             top25 = sum(head(total, 20)), 
#             cnt = n()) %>% 
#   mutate(top5_share = top5/year_sum, 
#          top25_share = top25/year_sum)
# e_fig$top_rec_loans
# 
# e_fig$top_rec_loans %>% 
#   write.xlsx("output/PRESS/charts PRESS 2023/top recipients loans.xlsx")

# 2. fragile states
## 2.1 timeseries of funding to fragile stats
e_fig$fs_ts <- x %>% 
  mutate(fs =replace_na(fs, 0)) %>% 
  mutate(fs = fs == 1) %>% 
  # filter(fs) %>% 
  # filter(year > 2018) %>% 
  filter(finance_t_name == "Standard grant") %>% 
  filter(countrySpecific) %>% 
  fun_summarise_ts("fs", T, 3) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  # mutate(share = share)
  select(group_year, 
         `Total amount` = `TRUE`, 
         `% of total country-specific grants` = share) 

## 2.2 fragile states top donors
var_year_start <- 2018
var_n_top <- 10
var_group <- "ch_name"
var_finance_type <- "grant"

# x %>%
#   mutate(fs =replace_na(fs, 0)) %>%
#   mutate(fs = fs == 1) %>%
#   filter(country_specific) %>%
#   filter(fs) %>%
#   fun_summarise_ts("finance_t_name")

e_fig$fs_top_donors <- x %>% 
  mutate(fs =replace_na(fs, 0)) %>% 
  mutate(fs = fs == 1) %>% 
  filter(country_specific) %>% 
  filter(fs) %>% 
  fun_name_change_4output(new_names = df_names_change) %>% 
  mutate(donors = ifelse(finance_t_name == "Standard loan", 
                         " (loans)", 
                         "")) %>% 
  mutate(donors = paste0(ch_name, donors)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   "donors", 
                   NA
                   # var_finance_type
                   # NA
                   # "loan"
                   )

e_fig$fs_top_donors
e_fig$fs_top_donors %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 3.3 top fs donors.xlsx")

## 2.3 fragile states recipients
var_n_top <- 14
var_group <- "recipientname"
var_finance_type <- "grant"

e_fig$fs_top_rec <- x %>% 
  filter(country_specific) %>% 
  mutate(fs =replace_na(fs, 0)) %>% 
  mutate(fs = fs == 1) %>% 
  filter(fs) %>% 
  fun_name_change_4output(new_names = df_names_change) %>% 
  mutate(recs = ifelse(finance_t_name == "Standard loan", 
                       " (loans)", 
                       "")) %>% 
  mutate(recs = paste0(recipientname, recs)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   "recs", 
                   NA)

e_fig$fs_top_rec
e_fig$fs_top_rec%>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 3.2 top fs recs.xlsx")



# 3. SIDS


df_sids <- read_csv("data/auxiliary/sids.csv")

### 2.3.1 SIDS country
x <- x %>% 
  left_join(df_sids) %>% 
  mutate(sids = !is.na(sids))


### 2.3.2 SIDS ergion
x <- x %>% 
  mutate(sids_region = regionname %in% c("Oceania", "Caribbean")) %>% 
  mutate(sids_region_recipient = recipientname %in% c("Caribbean, regional",
                                                      "Oceania, regional",
                                                      "Melanesia, regional")) %>% 
  mutate(sids_region_recipient = ifelse(is.na(recipientname), F, 
                                        sids_region_recipient), 
         sids_region = ifelse(is.na(regionname), F, sids_region)) %>% 
  mutate(sids_region = sids_region|sids_region_recipient) %>% 
  select(-sids_region_recipient) %>% 
  rename(sids_recipient = sids) %>% 
  mutate(sids = sids_recipient|sids_region)

rm(df_sids)

# 3.1 sids recipients
var_year_start <- 2018
var_n_top <- 10
var_group <- "recipientname"
var_finance_type <- NA

e_fig$sids_recipients <-  x %>% 
  filter(sids) %>%
  mutate(recipientname = ifelse(is.na(isocode), "Regional/SIDS Unspecified", recipientname)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   var_group, 
                   var_finance_type)
e_fig$sids_recipients 

e_fig$sids_recipients  %>% 
  write.xlsx("output/press/charts PRESS 2023/final_fig 3.4 sids top recs.xlsx")

# 3.2 sids timeseries
# 
# e_fig$sids_ts <- df_crs %>% 
#   filter(sids) %>% 
#   mutate(tmp_var = "SIDS") %>% 
#   fun_summarise_ts("tmp_var", T, 3) 
# 
# e_fig$sids_ts
# 
# e_fig$sids_ts %>% 
#   write.xlsx("output/press/charts PRESS 2023/sids ts.xlsx")
# 
# df_crs %>% filter(is.na(commitment_year))
# 
# e_fig$sids_ts_commit <- df_crs %>% 
#   filter(sids) %>% 
#   mutate(usd_disbursement_defl = usd_commitment_defl) %>% 
#   mutate(year = commitment_year) %>% 
#   mutate(tmp_var = "SIDS") %>% 
#   fun_summarise_ts("tmp_var", T, 3) 
# e_fig$sids_ts_commit
# 
# # 3.3 sids top donors
# 
# var_year_start <- 2018
# var_n_top <- 10
# var_group <- "ch_name"
# var_finance_type <- NA
# 
# 
# e_fig$sids_donors <-  x%>% 
#   filter(sids) %>%
#   # mutate(recipientname = ifelse(is.na(isocode), "Regional/SIDS Unspecified", recipientname)) %>% 
#   fun_top_partners(var_year_start, 
#                    var_n_top, 
#                    var_group, 
#                    var_finance_type)
# e_fig$sids_donors
# 
# e_fig$sids_donors  %>% 
#   write.xlsx("output/press/charts PRESS 2023/sids top donors.xlsx")
