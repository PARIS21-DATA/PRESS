rm(list = ls())

df_crs <- read_feather("output/CH/2023-09-15 PRESS 2023 data.feather")
df_crs_morecols <- read_feather("output/CH/2023-09-19 PRESS 2023 data.feather")
setdiff(names(df_crs_morecols),
        names(df_crs))

df_crs <- df_crs_morecols %>% 
  select(db_ref, 
         donor_type, 
         finance_t_name) %>% 
  inner_join(df_crs)
rm(df_crs_morecols)
# 
# df_crs %>% 
#   group_by(year) %>% 
#   summarise(total = sum(usd_disbursement_defl, na.rm = T))
# df_crs %>% 
#   arrange(ch_name) %>% 
#   select(ch_name) %>% 
#   unique %>% 
#   as.data.frame()

# df_crs <- df_crs %>% 
#   mutate(usd_disbursement_defl = ifelse((ch_name != "The World Bank") & year == 2021, 
#          usd_disbursement_defl*(682/740), 
#          usd_disbursement_defl)) #%>% 
  # group_by(year) %>% 
  # summarise(total = sum(usd_disbursement_defl, na.rm = T))
# df_crs <- read_feather("output/press/2023-10-17 PRESS data with climate markers2023.feather")
df_standard <- read_csv("Output/ch/PRESS2022_v42022-10-27.csv")
df_donor_types <- read_rds("data/auxiliary/reporters_types_2022.rds")
df_regions <- read_feather("data/auxiliary/new_regions_2023.feather")
df_ida <- read_csv("data/auxiliary/IDA status.csv")
df_aid_t_name <- read.xlsx("data/auxiliary/Aid type names 2023.xlsx")
df_fs <- read_feather("data/auxiliary/fragile states 2023.feather")
df_sdg_regions <- read_rds("~/dropbox/PARIS21/R/SDG_collection_2022/Data/Auxiliary/region_code.RDS")
df_sdg_regions_names <- read_rds("~/dropbox/PARIS21/R/SDG_collection_2022/Data/Auxiliary/m49 code and names.RDS")
# df_recipients <- read_rds("data/auxiliary/regions.rds")
df_crs_best <- read_feather("output/press/2023-10-17 PRESS data with climate markers2023.feather")

names(df_crs)
names(df_standard)

# df_regions %>% 
#   select(regioncode, 
#          regionname) %>% 
#   distinct %>% 
#   write.xlsx("data/auxiliary/regions_dac2ch_2fix_2023.xlsx")


# read.xlsx("data/auxiliary/regions_dac2ch_fixed_2023.xlsx") %>% 
#   # select(region_ch, 
#   #        regioncode_ch) %>% 
#   # distinct
#   inner_join(df_regions) %>% 
#   # nrow
#   write_feather("data/auxiliary/new_regions_2023.feather")


df_standard$bi_multi
df_crs$bi_multi
# no need to change bi_multi

df_standard$ReporterType %>% table
df_crs$donor_type %>% table
df_standard$role %>% table


df_crs %>% 
  select(regioncode, regionname, dac_regionncode, dac_regionname) %>% 
  unique

df_sdg_regions <- df_sdg_regions %>% 
  filter(region_type == "sdg_region") %>% 
  select(iso, region_code, m49) %>% 
  distinct %>% 
  left_join(select(df_sdg_regions_names, 
                   region_name = GeoAreaName, 
                   region_code = GeoAreaCode)) %>% 
  rename(sdg_region_code = region_code) %>% 
  select(-m49)
rm(df_sdg_regions_names) 

# df_crs_best <- df_crs_best %>% 
#   mutate(gen_rmnch2_narrow = gen_rmnch2&(gender == 1)) %>%
#   mutate(gen_final = gen_ppcode|gen_donor|gen_marker2|gen_channel|gen_agency|gen_title|gen_desc|gen_rmnch2_narrow) %>% 
#   select(-gen_rmnch2_narrow) %>%
#   ungroup %>% 
#   filter(gen_final) %>% 
#   mutate(total = paste(db_original_id, 
#                        year, 
#                        dac_donorname, 
#                        recipientname, 
#                        projectnumber, 
#                        purposecode, 
#                        sep = ";;")) %>% 
#   select(gen_final, total)

df_crs_output <- df_crs_new_2

# df_crs_output <- df_crs_output %>% 
#   ungroup %>% 
#   mutate(total = paste(db_original_id, 
#                        year, 
#                        dac_donorname, 
#                        recipientname, 
#                        projectnumber, 
#                        purposecode, 
#                        sep = ";;")) %>% 
#   left_join(df_crs_best) %>%
#   select(-total)

rm(df_crs_best)

df_crs_output <- df_crs_output %>% 
  mutate(countrySpecific = !is.na(isocode), 
         BilateralDonor = (donor_type == "Bilateral")) %>% 
  mutate(type_of_specific = ifelse(countrySpecific, 
                                   ifelse(BilateralDonor, 
                                          1, 2), 
                                   ifelse(BilateralDonor, 
                                          3, 4)))

df_crs_output <- df_donor_types %>% 
  select(-ReporterName) %>% 
  inner_join((df_crs_output)) 

df_crs_output <- df_regions %>% 
  select(dac_recipientcode = dac_recipientcode, 
         # dac_regionncode = regioncode, 
         region_ch, 
         regioncode_ch) %>% 
  inner_join(df_crs_output)

df_crs_output <- df_ida %>% 
  select(-Economy) %>% 
  rename(isocode = Code) %>% 
  gather(key = "year", value = "ida", -isocode) %>% 
  mutate(year = as.numeric(year), 
         ida = ida == 1) %>% 
  mutate(ida = replace_na(ida, F)) %>% 
  right_join(df_crs_output)

df_crs_output %>% 
  filter(is.na(aid_t))

df_crs_output <- df_crs_output %>% 
  left_join(df_aid_t_name)

df_crs_output <- df_crs_output %>% 
  mutate(income_class = ifelse(incomegroupname == "LDCs", 
                               "L", 
                               ifelse(incomegroupname == "Other LICs", 
                                      "L", 
                                      ifelse(incomegroupname == "LMICs", 
                                             "LM", 
                                             ifelse(incomegroupname == "UMICs", 
                                                    "UM", 
                                                    ifelse(incomegroupname == "MADCTs", 
                                                           "H", 
                                                           ifelse(incomegroupname == "Part I unallocated by income", 
                                                                  "Non-Specific",
                                                                  "Other")))))))

df_crs_output %>% 
  filter(income_class == "Other")

df_crs_output$finance_t_name %>% table
df_crs_output <- df_crs_output %>% 
  mutate(FinancingInstruments = ifelse(finance_t_name == "Standard grant", 
                                       "Grant", 
                                       ifelse(finance_t_name == "Standard loan", 
                                              "Loan", 
                                              "Other")) )

# df_crs_output <- df_crs_output %>% 
#   mutate(gen_rmnch2_narrow = gen_rmnch2&(gender == 1)) %>%
#   mutate(gen_final = gen_ppcode|gen_donor|gen_marker2|gen_channel|gen_agency|gen_title|gen_desc|gen_rmnch2_narrow) %>%
#   # mutate(gen_final = gen_ppcode|gen_donor|gen_marker2|gen_title|gen_desc|gen_rmnch2_narrow) %>% 
#   select(-gen_rmnch2_narrow)

df_crs_output <- df_crs_output %>% 
  left_join(df_fs)

df_crs_gender <- read_xlsx("output/ch/2023-11-07 PRESS 2023 for CH.xlsx") %>% 
  select(db_ref, gen_final = gend)

df_crs_output <- df_crs_output %>% 
  left_join(df_crs_gender) %>%
  mutate(gen_final = replace_na(gen_final, F))


df_crs_output <- df_crs_output %>% 
  mutate(completion_year = substr(completiondate,1,4), 
         completion_year = as.numeric(completion_year)) %>% 
  mutate(completion_year = ifelse(is.na(completion_year), 
                                  year,
                                  completion_year)) %>% 
  mutate(completion_year = ifelse(completion_year < year,
                                  year, 
                                  completion_year))

df_crs_output$completion_year %>% table

df_crs_output %>% 
  filter(as.numeric(completion_year) < year) %>% 
  select(completiondate, year)

df_crs_output$commitment_year %>% head



rm(df_aid_t_name)
rm(df_donor_types)
rm(df_fs)
rm(df_ida)
rm(df_regions)


# df_crs  %>% 
#   select(dac_recipientcode, 
#          dac_regionname, 
#          dac_regionncode) %>% 
#   distinct %>% 
#   arrange(dac_recipientcode) %>% 
#   group_by(dac_recipientcode) %>% 
#   filter(n() > 1)
# 
# df_crs %>%
#   anti_join(df_crs_output %>% select(db_ref)) %>%
#   # select(ch_name, ReporterId, usd_disbursement_defl, year)
#   select(ReporterName = ch_name, 
#          ReporterId) %>% 
#   mutate(ReporterType = "Multilateral") %>% 
#   rbind(df_donor_types) %>% 
#   write_feather("data/auxiliary/reporters_types_2023.feather")


df_crs_output <- df_crs_output %>% 
  select(db_ref, 
         db_original = db_original_id, 
         ProgramName = projecttitle,
         Objectives = longdescription, 
         ExpectedEndDate = completiondate, 
         ListRecip = recipientname, 
         institutions = agencyname, 
         # usd_disbursements = usd_disbursement,
         usd_disbursements = usd_disbursement, 
         # CommitmentDate = commitmentdate, 
         CommitmentDate = commitment_year, 
         endyear = completiondate, 
         RecipientCode = isocode, 
         RecipientCodeNumeric = dac_recipientcode, 
         # usd_commitment = usd_commitment, 
         usd_commitment_defl,
         usd_commitment = usd_commitment, 
         usd_disbursement_defl, 
         # usd_disbursement = usd_disbursement,
         commitmentcurrency = currencycode, 
         commitment = commitment_national, 
         disbursements = usd_disbursement, 
         ReporterName = ch_name, 
         ReporterId, 
         ReporterType, 
         # ReporterType = NA ,
         bi_multi, 
         reportedyear = year, 
         gen = gender, 
         gend = gen_final, 
         fs, 
         ida, 
         Region.ID = regioncode_ch, 
         RegionName = region_ch, 
         FinancingMechanism = flowname, 
         FinancingApproach = aid_t_name, 
         income_class, 
         FinancingInstruments, 
         countrySpecific, 
         BilateralDonor, 
         type_of_specific, 
         dac_regionname,
         dac_regioncode = dac_regionncode
         )

df_crs_output %>% 
  filter(gend) %>%
  group_by(reportedyear, FinancingInstruments) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = FinancingInstruments, 
         value = total)

df_crs_output %>% 
  filter(fs ==1, 
         reportedyear > 2018) %>%
  group_by(FinancingInstruments, ListRecip) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  as.data.frame

df_crs_output %>% 
  filter(ListRecip == "Cameroon") %>% 
  filter(reportedyear > 2018) %>% 
  select(reportedyear, fs, usd_disbursement_defl, FinancingInstruments) %>% 
  as.data.frame %>% 
  .$usd_disbursement_defl %>% sum
  

df_crs_output <- df_crs_output %>% 
  mutate(nsds = "Do not know", 
         role = "Donor", 
         source  = "CRS", 
         collaborators = NA, 
         usd_costestimate = NA, 
         oda = 1, 
         ignore_in_ranking = F, 
         support = NA)

df_crs_output <- df_crs_output %>% 
  mutate(usd_disbursement_defl = usd_disbursement_defl *1000000)

df_crs_output <- df_crs_output %>% 
  mutate(usd_disbursements = usd_disbursements *1000000, 
         usd_commitment = usd_commitment *1000000, 
         usd_commitment_defl = usd_commitment_defl *1000000)


df_crs$incomegroupname %>% table

setdiff(names(df_crs_output), names(df_standard))
setdiff(names(df_standard), names(df_crs_output))

# add other strandardised columns
## reporterType
## support
## fix gender
## update CH donor names for AIIB
# 
# df_crs_output <- df_crs_output %>%
#   left_join(df_sdg_regions %>% rename(RecipientCode = iso))




df_crs_output %>% 
  mutate(ida = ifelse(ida, 1, 0)) %>% 
  write.xlsx(paste0("output/ch/", 
                    Sys.Date(), 
                    " PRESS 2023 for CH.xlsx"))


df_crs_output %>% 
  mutate(ida = ifelse(ida, 1, 0)) %>% 
  write.xlsx(paste0("output/ch/", 
                    "current", 
                    " PRESS 2023 for CH.xlsx"))

df_crs_output %>% 
  select(db_ref)

df_crs_output %>% 
  group_by(reportedyear) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T))

source("code/20.0a adding classifications.R")

# df_press %>% filter(ReporterName == "France") %>% select(ProgramName) %>% 
#   head %>% 
#   write.csv("data/Analysis/text_france_output.csv", 
#             fileEncoding = "macintosh")
