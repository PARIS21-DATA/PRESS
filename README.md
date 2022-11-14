# PRESS

Due to file size, data files in `data/raw/crs` are not fully uploaded. You can download these data from [here](https://oecd-my.sharepoint.com/:f:/g/personal/yu_tian_oecd_org/ElcFbNhUusNKpUNn-LC5Kr4BxvQep1SEJ3lJ1u5xiX18AA?e=pwu3Fj). The .zip files are downloaded from [the OECD](https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1). 

**Update PRESS Methodology review (until 24/06/2022):**

Code:
  - `0.0x`: <br />
    `00.1 text_preparation_functions.R` (originally `00.1 functions.R`) contains helper functions for stemming and lemmatization. `00.2 functions_thilo.R` and `00.3 functions_yu.R` contain helper functions for the frequency based approach in the second step after the text detection used earlier (in script `04. create matrix.R`)
  - `01.x`: <br />
    added `01.3 language_statistics.R` which analyses the language distribution in the CRS data and provides basic information and statistics
  - `0.2 adding text markers.R`: <br />
    main additions: dropping French part of Canadian titles and descriptions, stringsdistance to provide option to use description combination instead of only long description, use long description and save in description_comb (later used for xgboost, maybe rename later) **if** long descr longer than 2 characters and destinct from project title (max_stingdist < 10)
  - `0.3 search_filter.R`: <br />
    main additions: minority language treatment by using keywords list saved in `Data/Final keyword lists/`, for fr and es: text detection of titles using keywords since sufficient amount of gender and stat projects available, for de: translation of long descriptions so it can analyzed with English projects, treatment of different combinations for gender_filter by saving the respective datasets in `Data/Gender permutations/`
  - `03.1 split.R`: <br />
  deleted since the step was superflous after changes to previous script
  - `04. create matrix.R`: <br />
  not changed substantially 
  - `05.x`: <br />
  `*_stat.R` and `*_gender.R` contain identical code except for the specification of different paths for storing results and application of different filters for marking the learning set (gender instead of stat), could me unified with stat/gender switch to change between classifications <br />
  visualization scripts create accuracy, precision and recall plot, word length distributions and histograms for donor and sector distribution (have to be called right after classification scripts, use the data frames therein) <br />
  `*_stat_gender.R`used for a direct classification of gender statistics (not touched in while, should be checked) 
  - `06. gender_dashboard.R`: <br />
  not functional, intended to create shiny dashboard to visualize gender permutation results
  - `06.1 gender_classification_XGB_tmp.R`: <br />
  classification for gender permutation results, only temporary for testing the different classifications 
  
  
 
