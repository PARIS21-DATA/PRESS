
load("./Analysis/iati_filtred.rds")



donors  = iati_filtered  %>% select(donorname, reporting.org.ref, reporting.org.type, reporting.org.type.code)

donors = unique(donors)

load("./analysis/reporters_2021.rdata")

names(reporters)


write.csv(donors, file = "./analysis/iati_reporters.csv", row.names = F)


write.csv(reporters, file = "./analysis/crspress_reporters.csv", row.names = F)



donors_fixed = read.csv("./analysis/iati_reporters_fixed.csv", stringsAsFactors = F)
names(donors_fixed)
names(reporters)

donors_fixed = donors_fixed %>%
  mutate(donorname = ifelse(donorname == "Bundesministerium f<9f>r wirtschaftliche Zusammenarbeit und Entwicklung (BMZ)", "Bundesministerium für wirtschaftliche Zusammenarbeit und Entwicklung (BMZ)", donorname))

donors_fixed$donorname[34] = "Bundesministerium für wirtschaftliche Zusammenarbeit und Entwicklung (BMZ)"


donors_fixed = donors_fixed %>%
  select(donorname, donorname_unified, donorcode, donor_type)

donors_fixed = unique(donors_fixed)

save(donors_fixed,file = "./analysis/iati_donors_2021.rds")

# dups1 = which(donors_fixed$donorname %>% duplicated())
# 
# donors_fixed$donorname[dups1]
# 
# dups1 = which(iati_filtered_n$db_ref %>% duplicated())
# 
# iati_filtered_n$donorname[dups1 = which(iati_filtered_n$db_ref %>% duplicated())]

iati_filtered = iati_filtered %>%
  merge(donors_fixed, by = c("donorname"))


iati_filtered = iati_filtered %>%
  select(-donorname)%>% 
  dplyr::rename(donorname = donorname_unified)

save(iati_filtered, file = "./analysis/iati_stats_donorsUnified.rds")


rm(list = ls())

load("./analysis/press_crs_merged_reduced.Rds")
load("./analysis/iati_stats_donorsUnified.rds")

press_donors = press %>%
  select(commitmentdate, donorname) %>%
  unique

names(press_donors)


iati_filtered_duplicates = iati_filtered %>%
  join(press_donors, type = "inner")

iati_filtered = iati_filtered %>%
  filter(!(db_ref %in% iati_filtered_duplicates$db_ref))


iati_filtered %>%
  group_by(commitmentdate) %>%
  filter(currency %in% c("USD","GBP","EUR"))%>%
  dplyr::summarise(total = sum(usd_commitment))

iati_filtered$source = "iati"
rm(iati_filtered_duplicates)
rm(press_donors)


save(iati_filtered,file = "./analysis/iati_stats_donorsUnified_removedDups.rds" )







