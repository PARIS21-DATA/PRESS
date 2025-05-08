import pandas as pd
import numpy as np
import os
import re
from rapidfuzz import fuzz, process
import spacy
import fasttext
import re
import unicodedata

# Needed to download spacy models first - execute in terminal
# python -m nltk.downloader wordnet
# python -m spacy download en_core_web_sm
# python -m spacy download fr_core_news_sm
# python -m spacy download es_core_news_sm
# python -m spacy download de_core_news_sm

# Load spaCy models
spacy_models = {
    'en': spacy.load('en_core_web_sm'),
    'fr': spacy.load('fr_core_news_sm'),
    'es': spacy.load('es_core_news_sm'),
    'de': spacy.load('de_core_news_sm'),
}

# Remove 'cuenta' as stopword in Spanish to preserve 'cuenta nacional'
spacy_models['es'].Defaults.stop_words.remove('cuenta')
# Remove 'mine' as stopword in English to preserve 'land mine'
spacy_models['en'].Defaults.stop_words.remove('mine')

# Load the fasttext language detection model
fasttext_model = fasttext.load_model("./models/lid.176.bin")  


def normalize_str(text: str) -> str:
    """
    Normalize a text string by cleaning unwanted characters and formatting.

    This function performs the following operations:
    - Replaces hyphens with spaces to split hyphenated words.
    - Removes punctuation and symbol characters, while preserving:
        - Letters (including accented and non-ASCII characters)
        - Digits
        - Apostrophes (')
    - Collapses multiple whitespace characters into a single space.
    - Trims leading and trailing whitespace.

    Parameters:
        text (str): The input string to normalize.

    Returns:
        str: A normalized version of the input string.
    """

    # Return None if the input is None
    if text is None:
        return None

    # Replace hyphens with spaces to split hyphenated words
    text = text.replace('-', ' ')
    
    # Replace punctuation/symbols with a space except apostrophes
    text = ''.join(
        ch if (unicodedata.category(ch)[0] not in ('P', 'S') or ch == "'") else ' '
        for ch in text
    )

    # Replace multiple spaces with a single space
    text = re.sub(r'\s+', ' ', text)

    # Lowercase the text
    text = text.lower()
    
    return text.strip()


def remove_accents(text: str) -> str:
    """
    Remove accents from characters in a string.
    This function uses the `unicodedata` library to normalize the text and remove accents.

    Parameters:
        text (str): The input string to process.

    Returns:
        str: The input string with accents removed.
    """
    if text is None:
        return None
    return ''.join(
        c for c in unicodedata.normalize('NFD', text)
        if unicodedata.category(c) != 'Mn'
    )


def detect_language(text: str) -> str:
    try:
        predictions = fasttext_model.predict(text, k=1)
        return predictions[0][0].replace("__label__", "")
    except Exception:
        return None
    

def lemmatize_str(text: str, lang: str, remove_stopwords: bool = False) -> str:
    """
    Lemmatize a given text using spaCy for the specified language and return a lemmatized string.

    Args:
        text (str): The input text to be lemmatized.
        lang (str): The language code (e.g., 'en', 'fr', 'es', 'de') for spaCy's language model.
        remove_stopwords (bool, optional): If True, stopwords will be removed from the lemmatized output. Defaults to False.

    Returns:
        str: A string of lemmatized tokens joined by spaces. If the language is not supported, the original text is returned.
    """
    if not isinstance(text, str):
        return None 
    
    if lang in spacy_models:
        doc = spacy_models[lang](text)
        
        if remove_stopwords:
            lemmas = [token.lemma_ if token.lemma_ and not token.lemma_ == '--' else token.text for token in doc if token.is_alpha and not token.is_stop]
        else:
            lemmas = [token.lemma_ if token.lemma_ and not token.lemma_ == '--' else token.text for token in doc if token.is_alpha]
        return " ".join(lemmas)
    else:
        # Fallback: Return the original text if the language is not supported
        return text
    

def lemmatize_batch(texts: list, lang: str, batch_size: int = 100, remove_stopwords: str = False) -> list:
    """
    Lemmatize a batch of texts using spaCy for the specified language.

    Args:
        texts (list): A list of input texts to be lemmatized.
        lang (str): The language code (e.g., 'en', 'fr', 'es', 'de') for spaCy's language model.
        remove_stopwords (bool, optional): If True, stopwords will be removed from the lemmatized output. Defaults to False.

    Returns:
        list: A list of lemmatized strings.
    """
    if lang not in spacy_models:
        # Fallback: Return original texts if the language is not supported
        return texts

    nlp = spacy_models[lang]
    docs = nlp.pipe(texts, batch_size=batch_size)  # Adjust batch_size for optimal performance
    lemmatized_texts = []

    for doc in docs:
        if remove_stopwords:
            lemmas = [token.lemma_ if token.lemma_ and not token.lemma_ == '--' else token.text for token in doc if token.is_alpha and not token.is_stop]
        else:
            lemmas = [token.lemma_ if token.lemma_ and not token.lemma_ == '--' else token.text for token in doc if token.is_alpha]
        lemmatized_texts.append(" ".join(lemmas))

    return lemmatized_texts


def detect_keywords(text: str, lang: str, keyword_df: pd.DataFrame) -> list:
    """
    Detect keywords in a given text based on the specified language and a DataFrame of keywords.

    Args:
        text (str): The input text to search for keywords.
        lang (str): The language code (e.g., 'en', 'de', etc.).
        keyword_df (pd.DataFrame): A DataFrame containing keywords for different languages.

    Returns:
        list: A list of matched keywords, or None if no matches are found.
    """
    
    if pd.isna(text):
        return None
    if lang not in keyword_df.columns:
        lang = 'en'  # Default to English if language is not in keywords_dict

    # Load the keywords for the given language
    keywords = keyword_df[lang]
    # Remove empty strings and NaN values from keywords
    keywords = keywords.dropna().unique()
    keywords = [kw for kw in keywords if isinstance(kw, str) and kw.strip() != ""]

    # Detect keywords in the text
    matches = [kw for kw in keywords if kw in text]

    # Additionally check for English keywords if the language is not English
    if lang != 'en':
        english_keywords = keyword_df['en']
        english_keywords = english_keywords.dropna().unique()
        english_keywords = [kw for kw in english_keywords if isinstance(kw, str) and kw.strip() != ""]  
        matches += [kw for kw in english_keywords if kw in text]

    if matches:
        return list(set(matches))  # Return unique matches
    else:
        return None
    
def detect_acronyms(text: str, lang: str, acronyms_df: pd.DataFrame) -> list:
    """
    Detect an acronym in a given text based on the specified language and a DataFrame of keywords.

    Args:
        text (str): The input text to search for keywords.
        lang (str): The language code (e.g., 'en', 'de', etc.).
        acronyms_df (pd.DataFrame): A DataFrame containing acronyms for different languages.

    Returns:
        list: A list of matched acronyms, or None if no matches are found.
    """
    
    if pd.isna(text):
        return None
    if lang not in acronyms_df.columns:
        lang = 'en'  # Default to English if language is not in keywords_dict

    # Load the keywords for the given language
    acronyms = acronyms_df[lang]
    # Remove empty strings and NaN values from keywords
    acronyms = acronyms.dropna().unique()
    # Add a leading and trailing space to each acronym to ensure exact matching
    acronyms = [" " + acr + " " for acr in acronyms if isinstance(acr, str) and acr.strip() != ""]

    # Detect keywords in the text with leading and trailing spaces to ensure exact matching also in the beginning and end of the text
    text = " " + text + " "
    matches = [acr for acr in acronyms if acr in text]

    # Additionally check for English acronyms if the language is not English
    if lang != 'en':
        english_acronyms = acronyms_df['en']
        english_acronyms = english_acronyms.dropna().unique()
        english_acronyms = [" " + acr + " " for acr in acronyms if isinstance(acr, str) and acr.strip() != ""]
        matches += [acr for acr in english_acronyms if acr in text]

    if matches:
        return list(set(matches)) # return unique matches
    else:
        return None
    

def process_keywords(keywords_df: pd.DataFrame, langauges: list = ['en', 'fr', 'es', 'de'], remove_stopwords: bool = False) -> pd.DataFrame: 
    """
    Processes a DataFrame of keywords by normalizing, lemmatizing, and adding de-accented versions.
    
    Args:
        keywords_df (pd.DataFrame): A DataFrame where each column represents a language (e.g., 'en', 'fr', 'es', 'de') 
                                    and contains keywords as strings.
    
    Returns:
        pd.DataFrame: The processed DataFrame with normalized, lemmatized, and de-accented keywords.
    """

    # Check that the dataframe has only the specified languages as columns 
    if not all(lang in keywords_df.columns for lang in langauges):
        raise ValueError(f"DataFrame must contain only the following columns: {langauges}, but found {keywords_df.columns.tolist()}")

    for lang in keywords_df.columns:
        if lang == 'de':  # Special handling for German: split on hyphens and lemmatize each part keeping capitalization, then normalize
            keywords_df[lang] = keywords_df[lang].map(lambda x: x.replace('-', ' ') if isinstance(x, str) else x)
            keywords_df[lang] = keywords_df[lang].map(lambda x: lemmatize_str(x, lang, remove_stopwords=remove_stopwords) if isinstance(x, str) else x)
            keywords_df[lang] = keywords_df[lang].map(lambda x: normalize_str(x) if isinstance(x, str) else x)
        else:
            keywords_df[lang] = keywords_df[lang].map(lambda x: lemmatize_str(normalize_str(x), lang, remove_stopwords=remove_stopwords) if isinstance(x, str) else x)

    # Add de-accented versions of the keywords
    for lang in keywords_df.columns:
        deaccented_col = keywords_df[lang].map(
            lambda x: remove_accents(x) if isinstance(x, str) else x
        )
        
        # Identify non-duplicate deaccented values
        new_values = deaccented_col[~deaccented_col.isin(keywords_df[lang])]
        
        # Create a new DataFrame for the deaccented values
        new_values_df = pd.DataFrame({col: [None] * len(new_values) for col in keywords_df.columns})
        new_values_df[lang] = new_values.values
        
        # Concatenate the new DataFrame to the original keywords DataFrame
        keywords_df = pd.concat([keywords_df, new_values_df], ignore_index=True)
    
    return keywords_df