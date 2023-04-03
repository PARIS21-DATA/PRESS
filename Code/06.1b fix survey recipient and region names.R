rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

# reading in the data from the survey 
####################################
## --- PRESS Data Preparation --- ##
####################################

# 0 Input::

path_input <- paste0("data/Intermediate/06.1a survey data cleaned 1st step_", 
                     year(Sys.Date())
                     ,".rds")

path_output_db_and_recs <- paste0("Data/Intermediate/06.1b survey corrected recipient and regions_", 
                                  year(Sys.Date())
                                  ,".rds")

path_output_survey_w_db_and_recs <- paste0("Data/Intermediate/06.1b survey merged with corrected recipient and regions_", 
                                  year(Sys.Date())
                                  ,".rds")

path_aux_country_correction <- "Data/auxiliary/survey_recipient_names_correction.rds"

path_aux_region <- "data/auxiliary/regions.rds"

df_survey <- read_rds(path_input)

# 1. create a df with db_ref and every single recipient in the list

df_survey <- df_survey %>% 
  rename(recipientname = recipient)

ls_survey_recipients <- lapply(df_survey$recipientname ,FUN = function(x) str_split(x, pattern = ",\\s+"))
ls_survey_n.of.recipients <- sapply(ls_survey_recipients, FUN = function(x)  length(x[[1]])  ) %>% unlist
ls_survey_db_ref_unlist <- rep(df_survey$db_ref , ls_survey_n.of.recipients)
ls_survey_recipients_unlist <- unlist(ls_survey_recipients)


df_survey_recipients <- tibble(db_ref = ls_survey_db_ref_unlist, 
                               recipientname = ls_survey_recipients_unlist)

rm(ls_survey_recipients, ls_survey_recipients_unlist, 
   ls_survey_db_ref_unlist, ls_survey_n.of.recipients)

# 2. fixing the compatibility between survey data and region data
#### note: the region data here is the press regional standard.


## 2.1 correct recipient names
#### each year it might be different because there will be new illegal names entered. In that case path_aux_country_correction may needs to be updated
df_survey_recipients_name_correction <- read_rds(path_aux_country_correction)
df_survey_recipients <- df_survey_recipients %>% 
  rename(recipientname_old = recipientname) %>% 
  left_join(df_survey_recipients_name_correction) %>% 
  mutate(recipientname = ifelse(is.na(recipientname_new), 
                                recipientname_old, 
                                recipientname_new))  %>% 
  select(-recipientname_new, 
         -recipientname_old)

rm(df_survey_recipients_name_correction)

## 2.2 attach region table

df_regions <- read_rds(path_aux_region)
names(df_regions)

df_survey_recipients_regions <- df_survey_recipients %>% 
  # select(-isocode) %>% 
  left_join(df_regions)

### 2.2.1 checking if there are rows without a region name
df_regions %>% 
  select(regioncode, regionname) %>% 
  unique

df_survey_recipients_regions %>% 
  filter(is.na(regioncode)) %>% 
  select(recipientname) %>% 
  unique

## 2.3 save the df of db_ref, recipient name and region names

saveRDS(df_survey_recipients_regions, file = path_output_db_and_recs)


# 3 summarise information of regions

df_survey_recipients_regions %>% names()

## 3.1 summarise the number of regions by db_ref
df_survey_tab_by_reg <- df_survey_recipients_regions %>% 
  group_by(db_ref, regionname, regioncode) %>% 
  summarise(cnt = n()) %>% 
  group_by(db_ref) %>% 
  mutate(cnt_regions = n())

## 3.2 filter out the ones with only one region
df_survey_tab_single_reg <- df_survey_tab_by_reg %>% 
  filter(cnt_regions <= 1) %>% 
  select(-cnt, -cnt_regions)

## deal with the one with multiple regions
### 3.3.1 filter out the ones with multiple region
df_survey_tab_multiple_reg <- df_survey_tab_by_reg %>% 
  filter(cnt_regions > 1) 
### 3.3.2 change region names to larger region to see if it can cover the different region names under the same project  
df_survey_tab_multiple_reg <- df_survey_tab_multiple_reg %>% 
  select(db_ref) %>% 
  left_join(df_survey_recipients_regions) %>% 
  select(db_ref, 
         regionname = regionname_larger, 
         regioncode = regioncode_larger) %>% 
  unique %>% 
  group_by(db_ref) %>% 
  mutate(cnt = n()) 
### 3.3.3 for the ones that cannot be summarised, the region would be unallocated
df_survey_tab_multiple_reg <- df_survey_tab_multiple_reg %>%
  mutate(regionname = ifelse(cnt >1, "Multilateral unallocated", regionname), 
         regioncode = ifelse(cnt>1, 1500, regioncode)) %>% 
  select(db_ref, regionname, regioncode) %>% 
  unique

## 3.4 merge the ones with single region names and multiple region names

df_survey_reg <- rbind(df_survey_tab_single_reg, df_survey_tab_multiple_reg) 

## 3.5 merge it back to the original survey data
df_survey_regions <- left_join(df_survey, df_survey_reg)

rm(df_survey_reg, 
   df_survey_tab_by_reg, 
   df_survey_tab_multiple_reg, 
   df_survey_tab_single_reg)

# 4. assign a single recipient code to each project

## 4.1 assign the iso and iso numeric code to the ones with only one recipient
df_survey_recipients_regions_unique.country <- df_survey_recipients_regions %>% 
  select(db_ref, isocode, iso3n
         # , recipientname
         ) %>%
  unique %>% 
  group_by(db_ref) %>% 
  mutate(cnt_isos = n()) %>% 
  filter(cnt_isos <= 1) %>% 
  select(-cnt_isos)
## 4.2 assign the dac recipient code to the ones with only one recipient

df_survey_recipients_regions_unique.dac.code <- df_survey_recipients_regions %>% 
  select(db_ref, dac_recipientcode
         # , recipientname
         ) %>%
  unique %>% 
  group_by(db_ref) %>% 
  mutate(cnt_isos = n()) %>% 
  filter(cnt_isos <= 1) %>% 
  select(-cnt_isos)

#### could be important to check if there are same number of rows here

## 4.3 merge them with the original survey data
df_survey_reg_rec_code <- df_survey_regions %>% 
  left_join(df_survey_recipients_regions_unique.country) %>% 
  left_join(df_survey_recipients_regions_unique.dac.code)

rm(df_survey_recipients_regions_unique.country, 
   df_survey_recipients_regions_unique.dac.code, 
   df_survey_recipients_regions, 
   df_survey_regions)

## 4.4 some testing data to be removed from the survey data

df_survey <- df_survey_reg_rec_code

names(df_survey)

# 5. save data

df_survey %>% 
  saveRDS(path_output_survey_w_db_and_recs)