stem_and_concatenate <- function(x_string, language = "en"){
  # this function can be improved if we remove duplicates from the beginning
  # partially resolved by always remove duplicates at the beginning of each script
  df_string <- x_string %>%
    data.frame(text = x_string, stringsAsFactors = F) %>%
    mutate(ref = 1:length(x_string))  
  
  x_string_stem <- df_string %>%
    unnest_tokens(word, text) %>%
    anti_join(get_stopwords(language = language), by = "word") %>%
    mutate(stem = wordStem(word, language = language)) %>%
    group_by(ref) %>%
    dplyr::summarise(text_stem = paste(stem, collapse = " ")) %>% # necessary to specify dplyr::, if plyr is loaded plyr's summarise() drops ref
    ungroup() %>%
    right_join(df_string, by = "ref") %>%
    mutate(text_stem = ifelse(is.na(text_stem), text, text_stem)) %>% # there are some empty titles
    arrange(ref) %>%
    .$text_stem
  
  return(x_string_stem)  
}
