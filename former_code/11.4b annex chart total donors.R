# 4. top donors 
var_year_start <- 2018
var_n_top <- 10
var_group <- "ch_name"
var_finance_type <- "grant"

# confirm which type of projects to use grant equiv
df_crs %>% 
  filter(grantequiv >= 0) %>% 
  filter(finance_t_name == "Standard loan") %>% 
  select(usd_disbursement_defl, grantequiv, finance_t_name, ch_name) 

df_crs$finance_t_name %>% table
df_crs %>% 
  mutate(usd_disbursement_defl = usd_disbursement_defl_equiv) %>% 
  filter(finance_t_name_w_equiv != "Shares in collective investment vehicles " ) %>% 
  fun_summarise_ts("finance_t_name_w_equiv")

# 4.1 top non-bilat
var_group <- "donors"
var_n_top <- 11
var_finance_type <- NA

e_fig$top_multi_donors <- df_crs %>% 
  filter(donor_type != "Bilateral") %>%
  fun_name_change_4output(new_names = df_names_change) %>% 
  mutate(donors = ifelse(finance_t_name == "Standard loan", 
                         " (loans)", 
                         "")) %>% 
  mutate(donors = paste0(ch_name, donors)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   var_group, 
                   # var_finance_type
                   NA
                   # "loan"
  )

e_fig$top_multi_donors

e_fig$top_multi_donors %>% 
  write.xlsx("output/press/charts PRESS 2023/overall top donors non-bilat.xlsx")

# 4.2 top bilat

var_n_top <- 10
e_fig$top_bi_donors <- df_crs %>% 
  filter(donor_type == "Bilateral") %>% 
  fun_name_change_4output(new_names = df_names_change) %>% 
  mutate(donors = ifelse(finance_t_name == "Standard loan", 
                         " (loans)", 
                         "")) %>% 
  mutate(donors = paste0(ch_name, donors)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   var_group, 
                   # var_finance_type
                   NA
                   # "loan"
  )
e_fig$top_bi_donors

e_fig$top_bi_donors %>% 
  write.xlsx("output/press/charts PRESS 2023/overall top donors bilat.xlsx")

df_crs %>% 
  filter(!grepl("Camp expenses in", 
               projecttitle, 
               T)) %>% 
  select(projecttitle) 

df_crs %>% 
  fun_name_change_4output(new_names = df_names_change) %>% 
  filter(ch_name == "United Arab Emirates",
         year> 2018) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  # select(usd_disbursement_defl, projecttitle, identified_by_stat) 
  select(longdescription) %>% 
  distinct %>% 
  as.data.frame()
