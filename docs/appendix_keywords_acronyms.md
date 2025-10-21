## Statistics

#### Keywords

{{ read_excel('../data/keywords/statistics_gender/keyword_statistics_gender.xlsx', usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

#### Acronyms

{{ read_excel('../data/keywords/statistics_gender/keyword_statistics_gender.xlsx', sheet_name="statistics acronyms", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

#### Blacklist

{{ read_excel('../data/keywords/statistics_gender/keyword_statistics_gender.xlsx', sheet_name="stat blacklist", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}


## Gender

#### Keywords

{{ read_excel('../data/keywords/statistics_gender/keyword_statistics_gender.xlsx', sheet_name="gender", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

#### Acronyms

{{ read_excel('../data/keywords/statistics_gender/keyword_statistics_gender.xlsx', sheet_name="gender acronyms", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}


## Topic Focus

#### AI 

{{ read_excel('../data/keywords/topic_focus/keywords_topic_focus.xlsx', sheet_name="AI", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

{{ read_excel('../data/keywords/topic_focus/keywords_topic_focus.xlsx', sheet_name="AI acronyms", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

#### DHS

{{ read_excel('../data/keywords/topic_focus/keywords_topic_focus.xlsx', sheet_name="DHS", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}

{{ read_excel('../data/keywords/topic_focus/keywords_topic_focus.xlsx', sheet_name="DHS acronyms", usecols=["id", "en", "fr", "es", "de"], keep_default_na=False, na_filter=False) }}
