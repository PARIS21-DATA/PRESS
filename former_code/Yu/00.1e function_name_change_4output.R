fun_name_change_4output <- function(x, new_names = df_names_change) {
  x <- left_join(x, new_names %>% rename(ch_name = original)) %>% 
    mutate(ch_name = ifelse(is.na(new), 
                            ch_name, 
                            new)) 
  return(x)
}
