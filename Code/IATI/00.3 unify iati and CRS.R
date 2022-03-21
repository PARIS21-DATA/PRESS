
rm(list = ls())
setwd("~/Dropbox/PARIS21/PRESS/2021/")
donor_crs = read.csv2("donorlist_DAC.csv", stringsAsFactors = F, sep = ",")
recipient_crs = read.csv2("recipientlist_DAC.csv", stringsAsFactors = F, sep = ",")

recipient_crs$DACrecipient = T




load("crs_2021_preclean.Rds")
write.csv(names(crs), file = "names_crs_2021_preclean.csv")

load("iati_2021_dateCleaned_lean.rds")

write.csv(names(iati),file = "names_iati_2021_dateCleaned_lean.csv" )


crs %>%
  select(recipientcode)%>%
  head

iati %>%
  select(recipient.country.code)%>%
  head

iati.rec.test = iati$recipient.country.code %>% table  %>% as.data.frame()

iati.rec.test = iati$recipient.country %>% table  %>% as.data.frame()

rm(iati.rec.test)

crs_rec_code_list = crs %>%
  select(recipientcode, recipientname) %>%
  unique

iati_recs = iati %>% 
  select(db_ref, recipient.country) %>%
  cSplit("recipient.country", ";","long") 

# iati_recs$recipient.country %>% table %>% as.data.frame()

iati_recs_unique = unique(iati_recs[,2]) %>%
  mutate(recipient.iso3c = countrycode( recipient.country,  "country.name", "iso3c" ))

iati_recs_unique = iati_recs_unique %>%
  mutate(recipient.iso3c = ifelse(recipient.country == "Kosovo", "XKX", recipient.iso3c)) %>%
  filter(!is.na(recipient.iso3c))

# iati_recs_unique[nrow(iati_recs_unique),] = 

names(recipient_crs)

iati_recs_unique = recipient_crs %>%
  filter(iso3c != "") %>%
  merge(iati_recs_unique , by.x = "iso3c" , by.y = "recipient.iso3c" , all.y = T) %>%
  select(iso3c, DAC_IncomeGroup, FragileStateDAC, DAC_IncomeGroup, FragileStateDAC, DACrecipient, recipient.country)

iati_recs_unique = iati_recs_unique %>%
  mutate(DACrecipient = ifelse(is.na(DACrecipient), F, DACrecipient) ,  
         FragileStateDAC = ifelse(is.na(FragileStateDAC), 0, FragileStateDAC), 
         DAC_IncomeGroup = ifelse(is.na(DAC_IncomeGroup),"Non_DAC", DAC_IncomeGroup)) 


iati_recs_unique = iati_recs_unique %>%
  mutate(region = countrycode(iso3c, "iso3c", "un.regionsub.name")) %>%
  mutate(region = ifelse(iso3c == "XKX", "Southern Europe", region)) %>%
  mutate(region = ifelse(iso3c == "TWN", "Eastern Asia", region)) %>%
  mutate(region = ifelse(iso3c == "ATA", "Antarctica", region)) 

# unique(iati_recs_unique$region) %>% write.csv("un_sub_region_list.csv")


rm(crs)

rm(donor_crs, crs_rec_code_list,recipient_crs)


#


regionList = read.csv("un_sub_region_list_updated.csv", stringsAsFactors = F) %>%
  filter(source == "UN") %>%
  select(-source)
# regionList[nrow(regionList)+1,] = c("","Unspecified")
iati_recs = iati_recs %>% 
  filter(recipient.country!= "", !is.na(recipient.country))
#

iati_recs = iati_recs %>%
  # select(-recipient.iso3c) %>%
  merge(iati_recs_unique, by = "recipient.country", all.x = T) 

iati_recs = iati_recs %>%
  merge(regionList, by = "region", all.x = T)


save.image(file = "iati_recs_splited.rdata")


#

#