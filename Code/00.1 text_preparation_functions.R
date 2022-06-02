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

stem_and_concatenate_tm <- function(string, language = "en") {
  string <- string %>% 
    tolower %>% 
    removeWords("'s") %>% # remove possesive s so that plural nouns get lemmatized correctly, e.g. "women's"
    removeNumbers() %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    stripWhitespace %>%  
    removeWords(c(stopwords(language))) %>% 
    #removeWords(c(stopwords(source = "smart")[!stopwords(source = "smart") %in% "use"])) %>% # exclude "use" from smart stopwords 
    stem_strings(language = language)

  return(string)
}

# # Try German
#library(udpipe)
#ud_model <- udpipe_download_model("german")
#ud_model <- udpipe_load_model(ud_model)
#
# # Try spiCy
# install.packages("spacyr")
# library(spacyr)
# # For using spacyr, version of miniconda necessary since the package is a wrapper
# # around a python packge, download miniconda here: https://docs.conda.io/en/latest/miniconda.html
# spacy_install()
# # spacy_finalize() # for finalizing a session
# spacy_initialize(model = "de_core_news_sm")

# german_lemma <- df_crs %>%
#   mutate(longdescription_clean = clean_and_lemmatize(longdescription, language =  "de"))
#
# clean_and_lemmatize(tolower(df_crs$longdescription[14]), language = "de")
# stem_and_concatenate(df_crs$longdescription[14], language = "de")

# Try translation
# table(df_crs$title_language)
# table(df_crs$long_language)


# Function to clean strings and lemmatize
clean_and_lemmatize <- function (string, language = "en"){
  if (!(language %in% c("en", "de"))) stop("No supported language chosen")
  
  string <- string %>% 
    tolower %>% 
    removeWords("'s") %>% # remove possesive s so that plural nouns get lemmatized correctly, e.g. "women's"
    removeNumbers() %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    stripWhitespace
  
  if (language == "en") {
    string <- string %>%
      removeWords(c(stopwords('english'))) %>% 
      removeWords(c(stopwords(source = "smart")[!stopwords(source = "smart") %in% "use"])) %>% # exclude "use" from smart stopwords 
      lemmatize_strings()
  
  } else if (language == "de") {
     string <- string %>%  
       enc2utf8() %>%
       udpipe_annotate(ud_model, .) %>%
       as.data.frame() %>%
       pull(lemma) %>% 
       paste(collapse = " ") %>%
       tolower %>%
       removeWords(c(stopwords('german'))) 
   }
  
  return(string)
}

