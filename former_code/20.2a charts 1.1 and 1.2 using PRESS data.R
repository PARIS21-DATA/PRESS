source("code/00.1b function_ts_summary.R")

e_fig <- new.env()

# fig 1: time series by donor type
e_fig$total_by_donorType <- df_crs_output_original_names%>% 
  filter(!bi_multi %in% c(2,3,7,8)) %>%
  # mutate(donor_type = ifelse(bi_multi ==2, 
  #                            "Multi-bi", 
  #                            donor_type)) %>% 
  # filter(gen_final, 
  #        finance_t_name != "Standard loan") %>% 
  group_by(year, donor_type) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key = donor_type, value = total) %>% 
  mutate(Total = sum(Bilateral, Multilateral, Private))


e_fig$total_by_donorType

e_fig$total_by_donorType %>% 
  write.xlsx("output/press/charts PRESS 2023/final_fig 1.1 time series by donor types.xlsx")

# fig 2: time series by grants and loans
e_fig$total_by_financeType <- df_crs_output_original_names %>% 
  filter(!bi_multi %in% c(2,3,7,8)) %>%
  filter(finance_t != 520) %>% 
  group_by(year, finance_t_name) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  finance_t_name, value = total) 

e_fig$total_by_financeType 

e_fig$total_by_financeType %>% 
  write.xlsx("output/press/charts PRESS 2023/final_fig 1.2 time series by grant and loans.xlsx")


# e_fig$multilat_by_financeType <- df_crs %>% 
#   filter(finance_t != 520) %>% 
#   filter(donor_type == "Multilateral") %>% 
#   group_by(year, finance_t_name) %>% 
#   summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
#   spread(key =  finance_t_name, value = total) 
# e_fig$multilat_by_financeType 

e_fig$total_by_financeType_by_donorType <- df_crs_output_original_names %>% 
  filter(!bi_multi %in% c(2,3,7,8)) %>%
  group_by(year, finance_t_name, donor_type)%>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T))

e_fig$total_by_financeType_by_donorType <- e_fig$total_by_financeType_by_donorType %>% 
  mutate(finance_t_name = str_replace(pattern = "Standard ",
                                      replacement = "", 
                                      string = finance_t_name)) %>% 
  mutate(finance_t_name = paste0(finance_t_name, "s")) %>% 
  mutate(finance_t_name = str_to_title(finance_t_name)) %>% 
  mutate(type = paste0(finance_t_name, ", ", 
                       donor_type)) 
  


e_fig$total_by_financeType_by_donorType %>% 
  filter(donor_type != "Private") %>% 
  ungroup %>% 
  select(year, type, total) %>% 
  spread(key = type, value = total ) # %>% 
  # write.xlsx("output/press/charts PRESS 2023/fig 1 time series by grant and loans by donor types.xlsx")


# 2.2 oda overall?

# df_crs_raw %>% 
#   filter(bi_multi != 4, bi_multi != 6) %>% 
#   select(finance_t) %>% 
#   table
# 
# df_crs_raw %>% 
#   filter(bi_multi != 4, bi_multi != 6) %>% 
#   fun_summarise_ts(group_var = "incomegroupname",
#                    rolling = T, 
#                    roll_period = 3)
# 
# df_crs_raw %>% 
#   filter(bi_multi == 4) %>% 
#   fun_summarise_ts(group_var = "incomegroupname",
#                    rolling = T, 
#                    roll_period = 3)
# 
# df_crs_raw %>% 
#   fun_summarise_ts("bi_multi")
# 
# 
# df_crs_raw %>% 
#   group_by(year, finance_t_name_father, donor_type) %>% 
#   summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
#   filter(donor_type == "Multilateral") %>% 
#   spread(key = finance_t_name_father, value =total) 



# fig 4 wb 
# df_crs %>%
#   mutate(group = ch_name == "The World Bank") %>% 
#   group_by(year, group) %>% 
#   summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
#   spread(key =  group, value = total)

