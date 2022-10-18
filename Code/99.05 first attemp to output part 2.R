source("code/99.04 first attemp to output.R")

df_press %>% .$commitmentdate %>% table 
df_press %>% .$commitmentdate %>% is.na %>% which
df_press %>% filter(is.na(commitmentdate)) %>% 
  select(source) %>% 
  table
df_press %>% filter(is.na(commitmentyear)) %>% 
  select(source) %>% 
  table

df_press <- df_press %>% 
  mutate(commitmentdate = ifelse(is.na(commitmentdate), year, commitmentdate))

# df_press$reported_year %>% is.na %>% which

df_press <- df_press %>% 
  mutate(reported_year = ifelse(is.na(reported_year), year, reported_year) , 
         reported_year = ifelse(is.na(reported_year), commitmentdate, reported_year) )

df_press <- df_press %>% 
  filter(reported_year > 2010)

df_press$commitmentdate %>% table

df_press$isocode %>% is.na %>% which
df_press %>% filter(is.na(isocode)) %>% select(recipientname) %>% unique


df_isocodes <- df_press %>% 
  mutate(isocode_new  = countrycode(recipientname, "country.name","iso3c")) %>% 
  mutate(isocode = ifelse(is.na(isocode), isocode_new, isocode)) %>% 
  select(db_ref, isocode)

df_press<- df_press %>% 
  select(-isocode) %>%
  left_join( df_isocodes) %>% 
  mutate(RecipientCode = isocode)

df_regions <- read_rds("data/auxiliary/regions.rds")

# #### ???? tbd
df_regions <- df_regions %>%
  select(regioncode ,
         regionname_larger) %>%
  unique
# 
df_regionnames_final <- df_press %>% select(db_ref, regioncode) %>%
  left_join(df_regions) %>% 
  rename(regionname = regionname_larger)
# 
# df_press %>% select(regionname)



df_press_reduced = df_press %>%
  mutate(RegionName = countrycode(RecipientCode, "iso3c", "region")) %>%
  mutate(RegionName = ifelse(is.na(RegionName), "Unallocated", RegionName))

a = df_press_reduced$RegionName %>% unique
a
b = c("Regional Unspecific","Eastern Europe", "Africa", "Asia Pacific", "Latin America and Caribbean","Asia Pacific", "Africa", "Latin America and Caribbean")

region_country_code_replace = data.frame(RegionName = a, RegionName_new = b, stringsAsFactors = F)

rm(a, b)

df_press_reduced = df_press_reduced %>%
  left_join(region_country_code_replace) %>%
  select(-RegionName) %>%
  dplyr::rename(RegionName = RegionName_new) 

df_press_reduced$RegionName %>% unique

# df_press_reduced <- df_press_reduced %>% 
#   left_join(df_regionnames_final) %>% 
#   select(-regioncode) %>% 
#   mutate(RegionName = ifelse(is.na(regionname), RegionName, regionname)) 

df_press_reduced <- df_press_reduced %>% 
  select(all_of(cols_to_sel), RegionName)


names(df_press_reduced)[1:length(cols_to_sel)] = col_names_final


# df_press_reduced <- df_press_reduced  %>% 
#   select(-regionname)

# df_press_reduced %>%  filter(is.na(RegionName)) %>% nrow

df_press_reduced$RegionName %>% table

# 
# df_press_reduced <- df_press_reduced %>% 
#   mutate(RegionName = ifelse(RegionName == "Asia-Pacific", "Asia Pacific", RegionName), 
#          RegionName = ifelse(RegionName == "Bilateral unallocated", "Regional Unspecified", RegionName), 
#          RegionName = ifelse(RegionName == "Multilateral unallocated", "Regional Unspecified", RegionName), 
#          RegionName = ifelse(RegionName == "Regional Unspecified", "Regional Unspecific", RegionName))

# rm(region_country_code_replace)



fs = read.csv("data/Analysis/fragile states_2021.csv", stringsAsFactors = F)

fs = fs %>%
  tidyr::gather(year,status, -Countries)
fs$year =
  substr(fs$year,2, 5) 

fs = fs %>%
  mutate(commitmentdate = as.numeric(year)) %>%
  filter(!is.na(status)) %>%
  select(-year)

fs$code = countrycode(fs$Countries, "country.name", "iso3c")

fs$code[which(fs$Countries == "Kosovo")] = "XKX"



fs = fs %>%
  select(ListRecip = Countries, RecipientCode = code, CommitmentDate = commitmentdate, fs = status)
names(df_press_reduced)
names(fs)
df_press_reduced = df_press_reduced %>%
  left_join(select(fs, -ListRecip)) 

# df_press_reduced$RecipientCode
# fs$RecipientCode
df_press = df_press_reduced




rm(press_reduced)

rm(fs)


ida = read.csv("./data/analysis/IDA status.csv",stringsAsFactors = F)  %>%
  select(-Economy) %>%
  dplyr::rename( RecipientCode= Code) %>%
  tidyr::gather( CommitmentDate, status, -RecipientCode)  %>%
  filter(!is.na(status)) %>%
  mutate(CommitmentDate = substr(CommitmentDate, 2, 5))  %>%
  mutate(CommitmentDate = as.numeric(CommitmentDate))

oda = read.csv("data/analysis/ODA.csv", stringsAsFactors = F) %>%
  mutate(RecipientCode = countrycode(Country, "country.name","iso3c"))

oda$RecipientCode[oda$Country == "Kosovo"] = "XKX"
oda$RecipientCode[oda$Country == "Micronesia"] = "FSM"

oda = oda %>%
  dplyr::rename(CommitmentDate = Year)

oda = oda %>%
  select(-Country) %>%
  mutate(oda = 1)

income_class = read.csv("data/Analysis/incomegroup_history.csv", stringsAsFactors = F)

names(income_class)

income_class = income_class %>%
  select(ISO:X2020)

income_class = income_class %>%
  select(-country) %>%
  dplyr::rename(RecipientCode = ISO) %>%
  tidyr::gather(CommitmentDate, status, -RecipientCode)
unique(income_class$status)

income_class = income_class %>%
  mutate(status = ifelse(status %in% c("L","UM","H","LM"), status, "Not classified")) 


ida = ida %>% dplyr::rename(ida = status)

# oda = oda %>% dplyr::rename(oda = status)
income_class = income_class %>% dplyr::rename(income_class = status)
income_class = income_class %>%
  mutate(CommitmentDate = substr(CommitmentDate, 2, 5)) %>%
  mutate(CommitmentDate = as.numeric(CommitmentDate))



df_press = df_press %>%
  left_join(ida) %>%
  left_join(oda) %>%
  left_join(income_class)
# head(press$income_class)
# 
# press$income_class %>% unique
# 
# press %>% filter(is.na(ReporterType)) %>% select(ReporterName, source) %>% head(10)
# 
# press %>% filter(is.na(ReporterType)) %>% select(ReporterName, source) %>% nrow()
# 

# load("data/analysis/donornames_to_fullnames.rds")
# 
# df_press = df_press %>%
#   mutate(ReporterName == ifelse(ReporterName == "Arab Fund for Economic and Social Development ", "Arab Fund for Economic and Social Development", ReporterName))
# 
# press_no_donor_type = df_press %>% 
#   filter(is.na(ReporterType)) 
# 
# df_press$ReporterType %>% unique


df_press = df_press %>%
  mutate(countrySpecific = !is.na(RecipientCode),
         BilateralDonor = ReporterType %in% c("DAC","non-Dac"), 
         type_of_specific = ifelse(BilateralDonor, 
                                   ifelse(countrySpecific, 1, 3), 
                                   ifelse(countrySpecific, 2, 4)), 
         oda = ifelse(source == "CRS", 1, 
                      ifelse(is.na(oda), 0, oda)), 
         ida = ifelse(is.na(ida), 0, ida) , 
         income_class = ifelse(is.na(income_class), "Non-Specific",income_class) )

df_press$income_class %>% unique


df_press = df_press %>%
  mutate(ignore_in_ranking = (ida ==1 )&(oda == 1)&( income_class!="H")& ( income_class!="UM"))

rm(preess0, ida, income_class, oda, press_no_donor_type, reporters, reporters_crs, reporters_notes)



df_press <- df_press  %>% 
  mutate(RegionName = ifelse(RegionName == "Asia-Pacific", "Asia Pacific", RegionName), 
         RegionName = ifelse(RegionName == "Bilateral unallocated", "Regional Unspecified", RegionName), 
         RegionName = ifelse(RegionName == "Multilateral unallocated", "Regional Unspecified", RegionName), 
         RegionName = ifelse(RegionName == "Regional Unspecified", "Regional Unspecific", RegionName))

regions = read.csv("data/analysis/region names and ids.csv", stringsAsFactors = F)
df_press$RegionName %>% unique

df_press  = df_press %>%
  select(-Region.ID) %>%
  left_join(regions)
names(regions)

a = df_press$Region.ID %>% is.na() %>% which
df_press$RegionName[a]
# select(press, ignore_in_ranking, ListRecip) %>%head

df_press$ignore_in_ranking = !df_press$ignore_in_ranking

df_press %>% 
  select(ReporterId)

# 
# press = press %>% 
#   dplyr::filter(ProgramName != "TC AGGREGATED ACTIVITIES", 
#                 Objectives != "TC AGGREGATED ACTIVITIES")

# press %>% group_by(CommitmentDate) %>% dplyr::summarise(total = sum(usd_commitment))

df_press$ReporterName %>% unique



write_rds(df_press, file = "output/data2022_20221012.rds")

write.csv(df_press, file = "output/press2022_to be pushed.csv", row.names = F, 
          na = "", 
          # sep = "|",
          fileEncoding = "utf-8")

write.csv(df_press, 
          file = "output/press2022_to be pushed_mac.csv", 
          row.names = F, 
          na = "", 
          # sep = "|",
          fileEncoding = "macintosh")

# unique list for giorgi 
# press %>% select(ReporterName, ReporterType) %>% unique %>% write.csv("analysis/reporters_press2021_ch.csv", row.names = F)

# load("analysis/press_for_ch_2021.rds")
# press_new = press
# press = read.csv("analysis/press2021.csv", stringsAsFactors = F)
# 
# which(!press$db_ref %in% press_new$db_ref)
# 
# press_new %>%
#   filter(CommitmentDate > 2009) %>%
#   group_by(ReporterName) %>%
#   dplyr::summarise(sum = sum(usd_commitment, na.rm = T)) %>%
#   arrange(desc(sum))


# "The World Bank" %in% unique(press$ReporterName)
# 
# 
# press1 = press
# press1$dups = duplicated(press1$db_original) 
# 
# press1 = press1 %>%
#   filter(!dups)
