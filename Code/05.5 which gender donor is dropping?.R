rm(list = ls())
source("Code/00. boot.R")
start <- Sys.time()


df_crs <- read_rds("data/intermediate/crs05.3_onlystats_utf8_full.rds")

print_time_diff(start)


df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  # filter(year > 2016) %>% 
  # filter(gender_filter_both_rmnch) %>%
  filter(stats_wo_infoSys) %>% 
  summarise(total = sum(usd_disbursement_defl),
            total = total *1000000, 
            # commit = sum(usd_commitment_defl)*1000000,
            cnt = n())



df_crs %>% group_by(commitmentyear
                    # , text_filter_gender
) %>% 
  filter(commitmentyear > 2010) %>%
  # filter(gender_filter_both_rmnch) %>%
  filter(stats_wo_infoSys) %>% 
  summarise(total = sum(usd_commitment_defl),
            total = total *1000000,
            # commit = sum(usd_commitment_defl)*1000000,
            cnt = n()) %>% 
  write_csv(file = "Output/commitments.csv")



# found and issue: information system should not be included
# create a script to fix it


print_time_diff(start)
# Funding to gender data
# FULL filter ex rmnch
df_crs %>% group_by(year
                    # , text_filter_gender
                    ) %>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl), 
            commit = sum(usd_commitment_defl),
            cnt = n()) 


df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl)
            # commit = sum(usd_commitment_defl),
            ) %>% 
  filter(total > 1) %>% 
  spread(key = year, value = total, fill = 0)


df_crs %>% 
  filter(donorname == "Food and Agriculture Organisation") %>% 
  filter(gender_filter_both_rmnch) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl)
            # commit = sum(usd_commitment_defl),
  )

df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname != "Food and Agriculture Organisation") %>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl), 
            commit = sum(usd_commitment_defl),
            cnt = n()) 

df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(year > 2014, year < 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl)
            # commit = sum(usd_commitment_defl),
  ) %>% 
  filter(total > 1) %>% 
  spread(key = year, value = total)



df_crs %>% group_by(year
                      # , text_filter_gender
  ) %>% 
  filter(!donorname %in% c("Food and Agriculture Organisation", 
                          "United States") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl), 
            commit = sum(usd_commitment_defl),
            cnt = n())   

# removing USA and FAO, see who has the biggest change
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(!donorname %in% c("Food and Agriculture Organisation"#, 
                           #"United States"
                           ) )%>% 
  filter(year > 2018) %>%
  filter(gender_filter_both_rmnch) %>%
  summarise(total = sum(usd_disbursement_defl))  %>% 
  spread(key = year, value = total, fill = 0) %>% 
  mutate(ratio = `2020`/`2019`) %>% 
  arrange(ratio) %>% 
  # as.data.frame() %>% 
  filter(ratio < 1)

# we should investigate Canada,  Sweden, UNICEF, USA,EU. In that order


## canada first
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "Canada") )%>% 
  filter(year > 2018) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl))
# save
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "Canada") )%>% 
  filter(year > 2018) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  write_csv("Data/Analysis/Canada_gender_2019_2020.csv")

## now do sweden
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "Sweden") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl))
# save
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "Sweden") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  write_csv("Data/Analysis/Sweden_gender_2019_2020.csv")

## now do UNICEF
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "UNICEF") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl))
# save
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "UNICEF") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  write_csv("Data/Analysis/UNICEF_gender_2019_2020.csv")


# the strange case of FAO
df_crs %>% 
  filter(donorname == "Food and Agriculture Organisation") %>% 
  filter(gender_filter_both_rmnch) %>% 
  filter(year == 2018) %>% 
  select(projecttitle, usd_disbursement_defl, gender)


## now USA
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "United States") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl, gender, 
         gen_donor, gen_marker2, gen_rmnch) %>% 
  arrange(desc(usd_disbursement_defl))
# save
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "United States") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  write_csv("Data/Analysis/United States_gender_2019_2020.csv")


## now UK
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "United Kingdom") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl, 
         # gender, 
         # gen_donor, gen_marker2, gen_rmnch
         ) %>% 
  arrange(desc(usd_disbursement_defl))

df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "United Kingdom") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl, 
         # gender, 
         # gen_donor, gen_marker2, gen_rmnch
  ) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  slice(1) %>% 
  as.data.frame()
# save
df_crs %>% group_by(year, donorname
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c(#"Food and Agriculture Organisation", 
    "United Kingdom") )%>% 
  filter(year > 2017) %>%
  filter(gender_filter_both_rmnch) %>%
  select(projecttitle, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  write_csv("Data/Analysis/United Kingdom_gender_2019_2020.csv")




# check donornames
df_crs %>% arrange(donorname) %>% select(donorname) %>%  unique()

# UK and sweden's contribution
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_rmnch) %>%
  summarise(total_disb = sum(usd_disbursement_defl), 
            total_commit = sum(usd_commitment_defl), 
            cnt = n()) 


df_crs$gen_rmnch_narrow %>% table


