

######
# treatment with information lost
df_crs <- df_crs %>% 
  filter(stats_wo_infoSys)
df_crs$commitmentdate = df_crs$commitmentyear


#####
# treatment without information lost

df_crs$usd_commitment = df_crs$usd_commitment *1000000
df_crs$usd_disbursement = df_crs$usd_disbursement *1000000
df_crs$usd_commitment_defl = df_crs$usd_commitment_defl *1000000
df_crs$usd_disbursement_defl = df_crs$usd_disbursement_defl *1000000
df_crs$commitment_national = df_crs$commitment_national * 1000000

df_crs$source = "CRS"
df_survey$source = "survey"


#######
# dealing with disbursement issues

df_survey %>%  filter(disbursement !="") %>% select(disbursement) %>%  head

ls_disbursements <- lapply(df_survey$disbursement , 
       FUN = function(x) str_split(x,  patter = "; ")) 

ls_n_disb <- lapply(ls_disbursements, 
                    FUN = function(x) length(x[[1]]) ) %>%  unlist
table(ls_n_disb)

ls_db_ref <- rep(df_survey$db_ref, ls_n_disb)
vec_disb <- unlist(ls_disbursements) 
ls_disb_split <- str_split(vec_disb, pattern =  "-") 
ls_disb_split[[7]] %>% length
ls_n_disb_split <- lapply(ls_disb_split, 
                          FUN = function(x) length(x) ) %>%  unlist 
table(ls_n_disb_split)


ls_disb_split <- ls_disb_split %>% unlist 
ls_disb_split <- ls_disb_split[ls_disb_split!=""]
n_data <- length(ls_disb_split)/3

disb_year <- ls_disb_split[(1:n_data)*3-2] %>% as.numeric
disb_amount <- ls_disb_split[(1:n_data)*3-1] %>% as.numeric
disb_currency <- ls_disb_split[(1:n_data)*3] %>% as.character
table(disb_currency)

df_survey_disb <- data.frame(disbursementdate = disb_year, 
                             disbursement = disb_amount, 
                             disbursement_currency = disb_currency) %>% 
  mutate(db_ref = ls_db_ref[ls_n_disb_split >1])

table(ls_n_disb) 


######
# treatment with information lost
df_survey_disb_simplified <- df_survey_disb %>% 
  group_by(db_ref) %>% 
  mutate(disbursement_sum = sum(disbursement), 
         year_max = max(disbursementdate)) %>% 
  filter(disbursementdate == year_max) %>% 
  select(disbursement = disbursement_sum, 
         disbursement_currency, 
         disbursementdate, db_ref) %>% 
  filter(disbursement_currency == "US Dollars")


df_survey <- df_survey %>% 
  select(-disbursement) %>% 
  left_join(df_survey_disb_simplified) %>% 
  mutate(usd_disbursement = disbursement, 
         usd_disbursement_defl = disbursement) 

df_survey = df_survey %>% 
  mutate(year = ifelse(is.na(disbursementdate), startyear, disbursementdate))

df_survey$year %>% is.na %>% which

######
# treatment without information lost
df_crs$reported_year = df_crs$year


press_commitments = df_survey %>%
  select(db_ref, usd_commitment, usd_costestimate) 

press_commitments$usd_commitment = gsub("[[:space:]]","", press_commitments$usd_commitment)

press_commitments$usd_commitment = gsub(",", "",press_commitments$usd_commitment)

press_commitments$usd_commitment = as.numeric(press_commitments$usd_commitment)

press_commitments = press_commitments %>%
  mutate(usd_commitment = ifelse(is.na(usd_commitment), 0, usd_commitment)) %>%
  mutate(usd_commitment = ifelse(usd_commitment == 0, usd_costestimate, usd_commitment))

df_survey  = df_survey %>% 
  select(-usd_commitment, -usd_costestimate) %>%
  left_join(press_commitments, by = "db_ref")



######
# treatment with information lost
df_survey = df_survey %>% 
  mutate(usd_disbursement_defl = ifelse(is.na(usd_disbursement_defl), usd_costestimate, usd_disbursement_defl))





merged_press_CRS = plyr::rbind.fill(df_survey, df_crs)

write_rds(merged_press_CRS, file = "./data/intermediate/99.01 merged_press_crs_2021_v2.Rds")


# sum(merged_press_CRS$usd_disbursement, na.rm= T)
# sum(merged_press_CRS$usd_commitment, na.rm= T)
# v2 removed the fuzzy part of japan


