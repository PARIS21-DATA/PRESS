# fig 5 gender


e_fig$gender_by_fintype <- x %>% 
  filter(gen_final) %>% 
  filter(finance_t_name == "Standard grant") %>%
  # filter(gender_filter_both_desc|gen_rmnch2|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("donor_type") %>% 
  mutate(total = sum(Bilateral, Multilateral, Private, na.rm = T)) %>% 
  relocate(total, .after = "year")
  # group_by(year) %>% 
  # summarise(Total = sum(usd_disbursement_defl, na.rm = T))

e_fig$gender_by_fintype

e_fig$gender_by_fintype %>% 
  write.xlsx("output/press/charts PRESS 2023/final_fig 2.1 gender by year.xlsx")





# top gender donors

var_year_start <- 2018
var_n_top <- 10
var_group <- "ch_name"
var_finance_type <- NA

# df_crs %>% 
#   filter(gender_filter_both_desc|gen_socpro|gen_rmnch2_uncertain) %>% 
#   fun_summarise_ts("finance_t_name")

source("code/00.1c function_top_partners.R")
source("code/00.1e function_name_change_4output.R")
df_names_change <- read_xlsx("data/auxiliary/new_names_for_output.xlsx")



e_fig$gender_top_donors <- x %>% 
  # filter(country_specific) %>%
  # mutate(rmnch = ifelse(is.na(rmnch), 
  #                       0, 
  #                       rmnch)) %>% 
  # filter(gender_filter_both_desc|gen_socpro|gen_rmnch2_uncertain) %>% 
  filter(gen_final) %>% 
  fun_name_change_4output(new_names = df_names_change) %>%
  mutate(donors = ifelse(finance_t_name == "Standard loan", 
                         " (loans)", 
                         "")) %>% 
  mutate(donors = paste0(ch_name, donors)) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   "donors", 
                   # var_finance_type
                   # "grant"
                   NA
  ) 

e_fig$gender_top_donors 

e_fig$gender_top_donors %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 2.2 top gender donors.xlsx")

# df_crs %>% 
#   # filter(country_specific) %>%
#   filter(gen_final) %>% 
#   # filter(gender_filter_both_desc|gen_socpro|gen_rmnch2) %>% 
#   fun_top_partners(var_year_start, 
#                    var_n_top, 
#                    var_group, 
#                    # var_finance_type
#                    "loan"
#   )


var_group <- "recipientname"
var_finance_type <- NA
var_n_top <- 10

e_fig$gender_top_rec <- x %>% 
  filter(gen_final) %>% 
  mutate(recipientname = ifelse(countrySpecific, 
                                recipientname, 
                                "Regional or unspecified recipients")) %>% 
  # fun_name_change_4output(new_names = df_names_change) %>% 
  mutate(recs = ifelse(finance_t_name == "Standard loan", 
                       " (loans)", 
                       "")) %>% 
  mutate(recs = paste0(recipientname, recs)) %>% 
  # filter(country_specific) %>%
  # filter(gender_filter_both_desc|gen_socpro|gen_rmnch2) %>% 
  fun_top_partners(var_year_start, 
                   var_n_top, 
                   # var_group, 
                   "recs",
                   # var_finance_type
                   # "grant"
                   NA
  )

e_fig$gender_top_rec

e_fig$gender_top_rec %>% 
  write.xlsx("output/PRESS/charts PRESS 2023/final_fig 2.3 top gender recs.xlsx")

# var_group <- "ch_name"
# # var_finance_type <- "grant"
# df_crs %>% 
#   filter(gender_filter_both_desc|gen_socpro|gen_rmnch2) %>% 
#   filter(!country_specific) %>%
#   fun_top_partners(var_year_start, 
#                    var_n_top, 
#                    var_group, 
#                    var_finance_type)