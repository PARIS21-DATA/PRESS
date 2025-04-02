# fig 5 gender

e_fig$gender_by_fintype <- df_crs %>% 
  filter(gender_filter_both_desc|gen_rmnch2|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("finance_t_name")  

e_fig$gender_by_fintype

e_fig$gender_by_fintype %>% 
  write.xlsx("output/press/charts PRESS 2023/5 gender by year by loans.xlsx")
