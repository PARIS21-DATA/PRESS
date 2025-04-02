rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

# reading in the data from the survey 
####################################
## --- PRESS Data Preparation --- ##
####################################

#Input::

path_input <- paste0("data/Raw/Survey/PRESS_survey_2021.rds")
path_output_survey_data <- paste0("data/Intermediate/06.1a survey data cleaned 1st step_", 
                                  year(Sys.Date())
                                  ,".rds")
path_output_disb_data <- paste0("data/Intermediate/06.1a survey data disbursement info_", 
                                  year(Sys.Date())
                                  ,".rds")

df_survey <- read_rds(path_input)

## 0.1 create ids
df_survey <- df_survey %>% 
  mutate(db_ref = paste0("survey_", pressid)) %>%
  dplyr::rename(db_original_id = pressid) 

## 0.2 filter out ones without donor names
df_survey <-  df_survey  %>%
  rename(donorname = donor) %>% 
  filter(donorname!="",!is.na(donorname))


df_survey$disbursement %>% head #characters
df_survey$commitment %>% head # characters
df_survey$cost_estimate %>% head
is.numeric(df_survey$cost_estimate)
df_survey$disbursement %>% unique %>% length
df_survey$disbursement %>% unique %>% head



## 1. clean up

df_survey <- df_survey %>% 
  # replace empty strings with NA
  mutate(across(c( "commitment"), ~ ifelse(.x == "", NA, .x))) %>%
  # there is not currency for these two variables so they should be in USD
  mutate(usd_commitment = commitment, 
         usd_costestimate = cost_estimate) %>%
  # replace the commas in the string values
  mutate(across(c("usd_commitment"), ~ str_replace_all(string = .x, pattern = ",", replacement = "")))  %>% 
  mutate(across(c( "usd_commitment"), ~ as.numeric(.x))) # %>% 
  # mutate(across(c("usd_costestimate", "usd_commitment"), ~ replace_na(.x, 0)))

# df_survey %>% 
#   filter(is.na(usd_commitment), !is.na(commitment)) %>% 
#   select(usd_commitment, commitment)

# df_survey$usd_commitment <- df_survey$commitment
# is.numeric(df_survey$usd_commitment)
# press <- press[!press$ReporterId %in% c(0,67),] ## empty entries and PARIS21 test entries
# df_survey$usd_commitment[is.na(df_survey$usd_commitment)] <- 0
# df_survey$usd_costestimate <- df_survey$cost_estimate
# df_survey$usd_costestimate[is.na(df_survey$usd_costestimate)] <- 0

attributes(df_survey$commitment)$variable.labels <- "original commitment data collected from the survey"
attributes(df_survey$usd_commitment)$variable.labels <- "numerised original commitment data collected from the survey, without replaced NAs with 0"
attributes(df_survey$cost_estimate)$variable.labels <- "original cost estimate data collected from the survey"
attributes(df_survey$usd_costestimate)$variable.labels <- "original cost estimate data collected from the survey, without replaced NAs with 0"

# 2. column for usd disbursments

df_disb <- tibble(disb = unique(df_survey$disbursement)) %>% 
  mutate(id = 1:n()) 

## split the disbursements if there are multiple entries

## 2.1 first split strings to each entry, which will still contain year value and currency infos
ls_disb <- df_disb$disb %>% 
  sapply( str_split, pattern = ";\\s+") 

df_disb_split_batch <- tibble(disb_batch = unlist(ls_disb), 
                              id = rep(df_disb$id, sapply(ls_disb, length ))) 

df_disb <- df_disb %>% 
  inner_join(df_disb_split_batch)

df_disb <- df_disb %>% 
  filter(disb_batch != "")

rm(ls_disb)

## 2.2 then split the information each year to year, value and currency

df_disb_split_batch <- df_disb_split_batch %>% 
  select(disb_batch) %>% 
  filter(disb_batch != "") %>% 
  unique %>% 
  mutate(id_bath = n())  

ls_disb_split_2end <- df_disb_split_batch$disb_batch %>% 
  str_split(pattern = "[-]")  %>% 
  lapply(function(x) x[x!=""]) # a few rows has splitted some ""s 


# sapply(ls_disb_split_2end, length) %>% table
# which(sapply(ls_disb_split_2end, length) == 4)
# ls_disb_split_2end[which(sapply(ls_disb_split_2end, length) == 4)]


ls_disb_split_2end <- ls_disb_split_2end %>% 
  lapply(function(x) {names(x) = c("year", "value", "currency"); return(x)})
df_disb_split_2end <- bind_rows(ls_disb_split_2end) %>% 
  mutate(disb_batch = df_disb_split_batch$disb_batch)

# ls_disb_split_2end[[1]]
# a <- ls_disb_split_2end[[1]]
# names(a) <- c("year", "value", "currency")
# ls_disb_split_2end %>% head

df_disb <- df_disb %>% 
  inner_join(df_disb_split_2end)

rm(
   ls_disb_split_2end, 
   df_disb_split_2end, 
   df_disb_split_batch)

## 2.3 split done, now format the values

df_disb <- df_disb %>% 
  mutate(value = as.numeric(value), 
         year = as.numeric(year)) %>% 
  select(-id) %>% 
  rename(disbursment_date = year, 
         disbursement = disb, 
         disbursement_currency = currency)

df_survey_disb <- inner_join(df_survey, df_disb)

df_survey_disb %>% 
  select(disbursement_currency) %>% 
  table

# 3. save results
saveRDS(df_survey, file = path_output_survey_data)
saveRDS(df_survey_disb, file = path_output_disb_data)

