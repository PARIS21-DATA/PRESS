fun_top_partners <- function(x, 
                             start_year, 
                             n_top, 
                             groupname, 
                             finance_type_filter = NA) {
  
  if(!is.na(finance_type_filter)) {
    x <-  filter(x, finance_t_name == paste0("Standard ", finance_type_filter)) 
  }

  
  x <- x %>% 
    filter(year > start_year) %>% 
    rename(group_var = groupname) %>% 
    group_by( group_var) %>%
    summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
    ungroup %>% 
    arrange( desc(total)) %>%
    mutate(partners = ifelse(row_number()> n_top, 
                             "Others", 
                             group_var), 
           order = ifelse(row_number()> n_top, 
                          n_top+1, 
                          row_number())) %>% 
    group_by(partners, order) %>% 
    summarise(total = sum(total, na.rm = T)) %>% 
    arrange(order) %>% 
    select(-order)
  return(x)
}
