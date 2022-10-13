


setwd("~/Dropbox/PARIS21/PRESS/2021/")
rm(list = ls())

load("./data/auxiliary/reporters.RData")
reporters = reporters %>%
  mutate(ReporterName = ifelse(donorname == "AfDF", "African Development Fund", donorname))

crsreporters2021 = read.csv("data/auxiliary/donorlist_DAC.csv", stringsAsFactors = F)

crsreporters2021 = crsreporters2021 %>%
  select(donorcode, donorname, donor_type) %>%
  arrange(donorcode)
# creporters$donorname[!(creporters$donorcode %in% crsreporters$donorcode)]
# 
# which(crspress$donorcode %in% c(912, 916))
# # no need to worry about "IDB Sp.Fund"        "AsDB Special Funds"

reporters_new = reporters %>%
  select(-donorname) %>%
  filter(!is.na(donorcode)) %>%
  right_join(crsreporters2021)

reporters = reporters_new %>%
  mutate(press = ifelse(is.na(ReporterName), 0, 1 ), crs = 1) 
reporters = reporters %>%
  select(ReporterId, 
         ReporterName, 
         donorname, 
         press,
         crs,
         donorcode,
         donor_type)
rm(reporters_new, crsreporters2021)




reporters  = reporters %>%
  mutate(donorname_unified = ifelse(crs == 1, donorname, NA)) %>%
  mutate(donorname_unified = ifelse(donorcode %in% c(901,903,905), "The World Bank", ifelse(donorcode %in% c(907, 958), "International Monetary Fund - IMF", donorname_unified) ))




survey_reporter_not_in_CRS = read.csv("./data/auxiliary//survey_reporter_not_in_CRS_2021_fixed.csv", stringsAsFactors = F)



survey_reporter_not_in_CRS = survey_reporter_not_in_CRS %>%
  mutate(donorname = NA, press = 1, crs = 0,  ReporterId = donorcode)

survey_reporter_not_in_CRS = reporters %>%
  select(donorcode, donorname_unified) %>%
  right_join(survey_reporter_not_in_CRS, by = "donorcode") 


survey_reporter_not_in_CRS = survey_reporter_not_in_CRS %>%
  mutate(donorname_unified = ifelse(is.na(donorname_unified), ReporterName, donorname_unified))

names(reporters)
names(survey_reporter_not_in_CRS)

reporters = rbind(reporters, survey_reporter_not_in_CRS)

reporters_notes = c("donorcode and donorname are for CRS projects, we changed all subsidiaries of WB (IDA, IFC, IBRD) and IMF (Conseesion trust fund) to WB and IMF, and the exact donor name is used the press ReporterName. ReporterID and ReporterName are for PRESS projects. The donor Type column is new this year.")



reporters %>% filter(is.na(donorname_unified)) %>%
  .$crs %>% table

# which(reporters$donorname_unified %>% is.na)
# 
# reporters = reporters %>%
#   mutate(donorname_unified = ifelse(is.na(donorname_unified), ReporterName, donorname_unified))



dup = reporters$donorcode %>% duplicated
dup_rev = rev(reporters$donorcode) %>% duplicated %>% rev

dup = dup | dup_rev


reporters_duplicated = reporters[dup, ]
rm(dup, dup_rev, reporters_duplicated, survey_reporter_not_in_CRS)
# 
# save(reporters, reporters_notes, file = "./analysis/reporters_2021.rdata")
