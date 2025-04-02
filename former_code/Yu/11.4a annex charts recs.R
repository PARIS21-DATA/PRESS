
df_names_change <- read_xlsx("data/auxiliary/new_names_for_output.xlsx")

fun_name_change_4output <- function(x, new_names = df_names_change) {
  x <- left_join(x, new_names %>% rename(ch_name = original)) %>% 
    mutate(ch_name = ifelse(is.na(new), 
                            ch_name, 
                            new)) 
  return(x)
}

# 1. top recipients
## 1.1 recipients of grants
df_crs_recipients <- df_crs %>% 
  filter(country_specific) %>% 
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

df_crs_recipients

df_crs_recipients %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/top recipients.xlsx")

## 1.2 recipients of loans
e_fig$top_rec_loans <- df_crs %>% 
  filter(country_specific) %>% 
  filter(finance_t_name == "Standard loan") %>% 
  group_by(year, recipientname) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  filter(total > 0) %>%
  arrange(year, desc(total)) %>% 
  group_by(year) %>% 
  summarise(year_sum = sum(total), 
            top5 = sum(head(total, 5)), 
            top25 = sum(head(total, 20)), 
            cnt = n()) %>% 
  mutate(top5_share = top5/year_sum, 
         top25_share = top25/year_sum)
e_fig$top_rec_loans

e_fig$top_rec_loans %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/top recipients loans.xlsx")

# 2. fragile states
## 2.1 timeseries of funding to fragile stats
e_fig$fs_ts <- df_crs %>% 
  # filter(fs) %>% 
  # filter(year > 2018) %>% 
  filter(finance_t_name == "Standard grant") %>% 
  filter(country_specific) %>% 
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

df_crs %>% 
  filter(country_specific) %>% 
  filter(fs) %>% 
  fun_summarise_ts("finance_t_name")

e_fig$fs_top_donors <- df_crs %>% 
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
  write.xlsx("output/PRESS/charts PRESS 2023/top fs donors.xlsx")

## 2.3 fragile states recipients
var_n_top <- 10
var_group <- "recipientname"
var_finance_type <- "grant"

e_fig$fs_top_rec <- df_crs %>% 
  filter(country_specific) %>% 
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
  write.xlsx("output/PRESS/charts PRESS 2023/top fs recs.xlsx")



# 3. SIDS
# 3.1 sids recipients
var_year_start <- 2018
var_n_top <- 10
var_group <- "recipientname"
var_finance_type <- NA

e_fig$sids_recipients <-  df_crs %>% 
  filter(sids) %>%
  mutate(recipientname = ifelse(is.na(isocode), "Regional/SIDS Unspecified", recipientname)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   var_group, 
                   var_finance_type)
e_fig$sids_recipients 

e_fig$sids_recipients  %>% 
  write.xlsx("output/press/charts PRESS 2023/sids top recs.xlsx")

# 3.2 sids timeseries

e_fig$sids_ts <- df_crs %>% 
  filter(sids) %>% 
  mutate(tmp_var = "SIDS") %>% 
  fun_summarise_ts("tmp_var", T, 3) 

e_fig$sids_ts

e_fig$sids_ts %>% 
  write.xlsx("output/press/charts PRESS 2023/sids ts.xlsx")

df_crs %>% filter(is.na(commitment_year))

e_fig$sids_ts_commit <- df_crs %>% 
  filter(sids) %>% 
  mutate(usd_disbursement_defl = usd_commitment_defl) %>% 
  mutate(year = commitment_year) %>% 
  mutate(tmp_var = "SIDS") %>% 
  fun_summarise_ts("tmp_var", T, 3) 
e_fig$sids_ts_commit

# 3.3 sids top donors

var_year_start <- 2018
var_n_top <- 10
var_group <- "ch_name"
var_finance_type <- NA

e_fig$sids_donors <-  df_crs %>% 
  filter(sids) %>%
  # mutate(recipientname = ifelse(is.na(isocode), "Regional/SIDS Unspecified", recipientname)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   var_group, 
                   var_finance_type)
e_fig$sids_donors

e_fig$sids_donors  %>% 
  write.xlsx("output/press/charts PRESS 2023/sids top donors.xlsx")
