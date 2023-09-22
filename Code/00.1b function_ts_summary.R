fun_summarise_ts <- function(x, group_var, rolling = F, roll_period = 3, start_year = 2010) {
  x <- x %>% 
    mutate(group_var = get(group_var))
  if(!rolling) {
    
    df_summary <-   x %>% 
      # mutate(group = group_var) %>% 
      group_by(year, group_var) %>% 
      summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
      spread(key =  group_var, value = total)
  } else {
    df_summary <- x %>% 
      # mutate(group = group_var) %>% 
      mutate(group_year = year
             ,
             group_year =  (group_year - start_year) %/% roll_period
             , group_year = group_year* roll_period,
             group_year_start = group_year + start_year,
             group_year_end = group_year + start_year + roll_period - 1, 
             group_year = paste0(group_year_start, "-", group_year_end)
      ) %>% 
      group_by(group_year, group_var) %>% 
      summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
      spread(key =  group_var, value = total)
  }
  return(df_summary)
}
