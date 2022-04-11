stem_and_concatenate <- function(x_string){
  # this function can be improved if we remove duplicates from the beginning
  # partially resolved by always remove duplicates at the beginning of each script
  df_string <- x_string %>%
    data.frame(text = x_string, stringsAsFactors = F) %>%
    mutate(ref = 1:length(x_string))  
  
  x_string_stem <- df_string %>%
    unnest_tokens(word, text)%>%
    anti_join(get_stopwords()) %>%
    mutate(stem = wordStem(word)) %>%
    group_by(ref) %>%
    summarise(text_stem = paste(stem, collapse = " ")) %>%
    right_join(df_string) %>%
    mutate(text_stem = ifelse(is.na(text_stem), text, text_stem)) %>% # there are some empty titles
    arrange(ref) %>%
    .$text_stem
  
  return(x_string_stem)  
}
