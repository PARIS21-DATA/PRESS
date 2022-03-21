rm(list =ls())


load("./analysis/iati_2021_marked_w_iso_region.rds")


names(iati) 

which(iati$countrySpecific.x!=iati$countrySpecific.y)

iati = iati %>%
  dplyr::rename(countrySpecific = countrySpecific.x) %>%
  select( -countrySpecific.y) 

names(iati)



iati = iati %>%
  dplyr::rename(
    db_original_id = iati.identifier,
    donorname = reporting.org, 
    projecttitle = title, 
    objectives = description, 
    recipientname = recipient.country, 
    recipientcode = iso3c, # careful
    #region,
    commitment = total.Commitment, # careful
    usd_disbursement = total.Disbursement, 
    commitmentdate = startyear
  )

iati$donorname %>% unique
iati$reporting.org.ref %>% unique %>% head(100)

is.numeric(iati$usd_commitment)

iati = iati %>%
  mutate(usd_commitment = as.numeric(commitment))  %>%  # careful
  filter(!is.na(usd_commitment))


iati %>% 
  filter(commitmentdate > 2010) %>%
  group_by(commitmentdate) %>%
  summarise(total = sum(usd_commitment))



save(iati, file = "./Analysis/iati_2021_marked_w_iso_region_fixedissues.rds")




# names(iati)


# iati %>% filter(is.na(usd_commitment_1)) %>% select(usd_commitment) %>% table














iati_orglist = read.csv("./analysis/OrganisationIdentifier_2021.csv", stringsAsFactors = F)

iati_donors= iati$donorname %>% unique

which(!(iati_donors %in% iati_orglist$name))

names(iati) == "donorname"




load("./data/regions.RData")
names(regions)
head(regions$isocode)
regions = regions %>%
  mutate(isocode = as.character(isocode))

names(regions)

load("./analysis/press_crs_merged_reduced.Rds")
names(press)


press$recipientcode %>% is.na() %>% sum
press$regionid %>% is.na() %>% sum
press$source %>% table
