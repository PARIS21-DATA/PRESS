
rm(list = ls())
load("./analysis/iati_2021_marked_w_iso_region_fixedissues.rds")
# projectIDs = iati %>% filter(allDAC) %>%
#   .$db_ref


projectIDs = iati %>% 
  # filter(allDAC) %>%
  .$db_ref

# load("iati_2021_dateCleaned_lean.rds")



iati$data = grepl("statis|estadi|data|datos|donnee|DHS|census|recensement|censo|survey|sondage|enquete|encuesta", iati$projecttitle, ignore.case = T)


iati$mine = grepl("land mine|small arm|demining|demine|landmine", iati$projecttitle, ignore.case = T)

iati$data2 = grepl("16062", iati$projecttitle, ignore.case = T)

iati$stats = (iati$data ) &(!iati$mine )


# iati$crvs = 

iati_filtered = iati %>% 
  filter(stats) 
rm(iati)


projectIDs = iati_filtered %>% 
  # filter(allDAC) %>%
  .$db_ref

save(iati_filtered, file = "./Analysis/iati_filtred.rds")
# is.numeric(iati$total.Commitment)
# is.character(iati$total.Commitment)
# 
# 
# iati_filtered$total.Commitment = as.numeric(iati_filtered$total.Commitment)

iati_filtered$currency %>% table

# iati_filtered$usd_commitment = iati_filtered$currn

# names(iati)

# iati_filtered = iati_filtered %>% 
#   filter(currency == "USD", db_ref %in% projectIDs) %>% 
#   arrange(desc(total.Commitment)) %>%
#   slice(-1)

iati_filtered %>% 
  filter(currency == "USD") %>%
  group_by(commitmentdate) %>%
  dplyr::summarise(amt = sum(usd_commitment) ) 


iati_filtered %>% 
  filter(currency == "USD", db_ref %in% projectIDs) %>%
  group_by(donorname) %>%
  dplyr::summarise(amt = sum(usd_commitment) )  %>%
  arrange(desc(amt))


iati$participating.org.ref..Funding. %>% unique %>% length



iati_filtered$sector %>% head

iati_filtered$sector %>% unique %>% length




rm(list = ls())
load("./analysis/iati_filtred.rds")
# sectors = iati_filtered$sector.vocabulary
# 
# sectors = sectors[order(nchar(sectors))]
# tail(sectors)

sectors = strsplit(iati_filtered$sector.vocabulary, ";")

sectors = lapply(sectors, FUN = function(x) {  x[!is.na(x)]}   )
sectors = lapply(sectors, unique   )
tail(sectors)
sectors %>% unlist %>% unique %>% length

sectors_ordered = sectors[rev(order(nchar(sectors)))]
head(sectors_ordered)

sectors %>% unlist %>% unique


iati_filtered$health = grepl("health|disease", iati_filtered$sector, ignore.case = T)

iati_filtered$covid = grepl("covid|corona", iati_filtered$objectives, ignore.case = T)



iati_filtered %>%
  filter(currency == "USD") %>%
  group_by(commitmentdate, health) %>%
  dplyr::summarise(amt = sum(usd_commitment) ) 
iati_filtered$usd_disbursement = as.numeric(iati_filtered$usd_disbursement)

iati_filtered %>%
  filter(nchar(sector)>10) %>%
  filter(currency %in% c("USD", "EUR","GBP") ) %>%
  group_by(covid, health) %>%
  dplyr::summarise(amt = sum(usd_disbursement, na.rm = T) ) %>%
  tidyr::spread(health, amt) %>%
  dplyr::rename(health = `TRUE`, othersectors = `FALSE`) %>%
  mutate(total = health + othersectors, 
         healthShare = health/total, 
         otherShare = othersectors/total)






iati$reporting.org.ref %>% unique %>% length
iati$donorname %>% unique %>% length


iati_filtered %>%
  filter(currency == "USD", db_ref %in% projectIDs) %>%
  arrange(desc(usd_commitment)) %>%
  head


iati_filtered %>% 
  filter(currency == "USD"|default.currency =="USD", db_ref %in% projectIDs) %>%
  arrange(desc(total.Commitment)) %>%
  select(title, reporting.org, total.Commitment, data, data2, startyear, endyear) %>%
  head

names(press_merged)

names(iati_filtered)

iati_to_merge = iati_filtered %>%
  select(
    db_original_id = iati.identifier,
    donorname = reporting.org, 
    projecttitle = title, 
    objectives = description, 
    endyear, startyear, 
    recipientname = recipient.country, 
    recipient.country.code = iso3c, 
    #region,
    usd_commitment = total.Commitment, 
    usd_disbursement = total.Disbursement, 
    commitmentdate = startyear, 
    countryspecifc = countrySpecific.x
         ) %>%
  mutate(source ="iati")

load("./analysis/press_crs_merged_reduced.Rds")
all_merged = rbind.fill(iati_to_merge,press)

all_merged %>% group_by(commitmentdate) %>%  dplyr::summarise(commitment = sum(usd_commitment,na.rm = T)) %>%as.data.frame()

all_merged %>% 
  # filter(commitmentdate>2000) %>%
  filter(!is.na(usd_commitment)) %>%
  group_by(commitmentdate, source) %>%  dplyr::summarise(commitment = sum(usd_commitment,na.rm = T)) %>%as.data.frame() %>%
  tidyr::spread(source, commitment)


all_merged %>% 
  # filter(commitmentdate>2000) %>%
  filter(!is.na(usd_commitment)) %>%
  group_by(commitmentdate) %>%  dplyr::summarise(commitment = sum(usd_commitment,na.rm = T)) %>%as.data.frame() 


all_merged = all_merged %>%
  mutate(commitmentdate = ifelse(commitmentdate>2019, 2019, commitmentdate)) 


# there is currently a gap of $130 million for year 2018, 2017, and a $130 million surplus for year 2015. 100 for year 2014. 
# should balance each other out. But you need to be very careful. 

# A 140 deficit for 2011 and 180 surplus for 2010. should balance out each other here



# all_merged = all_merged %>%
#   mutate(commitmentdate = ifelse(commitmentdate>2019, 2019, commitmentdate)) %>%
#   mutate(commitmentdate = ifelse(commitmentdate==2006, 2017, commitmentdate)) %>%
#   mutate(commitmentdate = ifelse(commitmentdate==2004, 2018, commitmentdate)) 
  
# all_merged %>% 
#   mutate(sour = substr(db_ref, 1, 4))%>%
#   filter(commitmentdate==2014) %>%
#   group_by(sour) %>%  dplyr::summarise(commitment = sum(usd_commitment,na.rm = T)) %>%as.data.frame()
# 
# 
# a = all_merged %>% 
#   filter(commitmentdate==2014) %>%
#   arrange(desc(usd_commitment)) %>% 
#   slice(-1,-2)
# 
# all_merged = all_merged %>%
#   filter(commitmentdate!=2014) %>%
#   rbind(a)



a = all_merged %>% 
  filter(commitmentdate==2012) %>%
  arrange(desc(usd_commitment)) %>%
  slice(-(1:10))

head(a$usd_commitment)
all_merged = all_merged %>%
  filter(commitmentdate!=2012) %>%
  rbind(a)


head(a$usd_commitment)  

all_merged = all_merged %>%
  filter(commitmentdate>2010) %>%
  mutate(reportedyear =ifelse(reportedyear < commitmentdate, commitmentdate, reportedyear))

all_merged %>% 
  # filter(commitmentdate>2000) %>%
  group_by(commitmentdate) %>%  dplyr::summarise(commitment = sum(usd_commitment,na.rm = T)) %>%as.data.frame()

press = all_merged
write.csv(press, file = "./analysis/press2021.csv", row.names = F )
save(press, file = "./analysis/press2021_v2.rds")

save.image(file = "first batch final 20210820.rdata")
