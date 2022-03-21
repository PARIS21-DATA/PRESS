

load(iati_rds_path)
names(iati)

a = gsub(names(iati),pattern = "[-]|[(]|[ ]|[)]",replacement = "_")
gsub(a,  pattern = "[^[:alnum:]]+",replacement = "_")

head(iati$start.planned)

unique(iati$iati.identifier) %>% length

iati$db_ref = paste0("crs_", iati$iati.identifier)

iati = iati %>%
  arrange(start.planned)

head(iati$start.planned)
which(iati$start.planned == "")
which(iati$start.planned == "") %>% length
which(iati$start.actual == "") %>% length

which(iati$end.planned == "") %>% length
which(iati$end.actual == "") %>% length


iati = iati %>%
  arrange(end.planned)
tail(iati$end.planned, 100)




iati = iati %>%
  mutate(start.planned.filled = ifelse(start.planned=="", start.actual, start.planned), 
         end.planned.filled = ifelse(end.planned=="", end.actual, end.planned), 
         end.planned.filled.further = 
           ifelse(end.planned.filled=="", start.planned.filled, end.planned.filled) )

iati = iati %>%
  mutate(start4 = substr(start.planned.filled, 1, 4) , 
         end4 = substr(end.planned.filled, 1, 4) , 
         end4.further = substr(end.planned.filled.further, 1, 4))

iati = iati %>%
  mutate(start4 = as.numeric(start4), 
         end4 = as.numeric(end4), 
         end4.further = as.numeric(end4.further), 
         end4.further.standard = ifelse(end4.further<2011|end4.further>2030, start4, end4.further))



table(iati$start4)
table(iati$end4)
table(iati$end4.further)
table(iati$end4.further.standard)

iati = iati %>%
  mutate(time.span = end4.further.standard - start4 )

table(iati$time.span)

iati = iati %>%
  mutate(
    end4.further.standard.negativeCorr = ifelse(time.span<0 , start4, end4.further.standard))

iati = iati %>%
  mutate(time.span.corr = end4.further.standard.negativeCorr - start4 )

iati$time.span.corr %>% table

save(iati, file = "iati_2021_dateCleaned.rds")


names(iati)

iati = iati %>%
  mutate(startyear = start4, 
         endyear = end4.further.standard.negativeCorr)

iati = iati %>%
  select(iati.identifier:db_ref, time.span, startyear, endyear) 

save(iati, file = "iati_2021_dateCleaned_lean.rds")




