rm(list = ls())
gc()
library(tidyverse)
library(plyr)
library(cldr)
setwd("~/Dropbox/PARIS21/PRESS/PRESS 2020/Analysis/")
path = "~/Dropbox/PARIS21/PRESS/PRESS 2020/CRS data/CRS "
list.files()
for(year in 2006:2018) {
  print(year)
  # ?read_table2
  # crstemp = read.table(paste0(path,year," data.txt"), sep = "|", stringsAsFactors = F, encoding = "UTF-8", fill = T, header = T)
  
  crstemp <- read.csv2(paste0(path,year," data.txt"), sep = "|", header = T, stringsAsFactors = F, encoding = "UTF-8")
  # beepr::beep(3)
  
  {
  # which(crstemp$DonorName == "Spain")
  # which(crstemp2$DonorName == "Spain")
  # crstemp$LongDescription[41714:90000] %>% head
  # crstemp$LongDescription[41714:90000] %>% head %>% detectLanguage %>% select(detectedLanguage)
  # crstemp2$LongDescription[128136 :130000] %>% head
  # crstemp2$LongDescription[128136 :130000] %>% head %>% detectLanguage %>% select(detectedLanguage)
  # crstemp2$LongDescription[128136 :130000] %>% head %>% cld2::detect_language()
  # 
  # # this is the reason why cldr package is not good
  # "REINSERCIÓN SOCIOLABORAL Y SOSTENIMIENTO DE RECURSOS SOCIALES PARA MUJERES VÍCTIMAS DE VIOLENCIA DE GÉNERO EN LAS PROVINCIAS DE MOSTAR Y NEVESINJE" %>% detectLanguage() 
  # 
  # library(cld3)
  # cld3::detect_language("REINSERCIÓN SOCIOLABORAL Y SOSTENIMIENTO DE RECURSOS SOCIALES PARA MUJERES VÍCTIMAS DE VIOLENCIA DE GÉNERO EN LAS PROVINCIAS DE MOSTAR Y NEVESINJE")
  # cld2::detect_language("REINSERCIÓN SOCIOLABORAL Y SOSTENIMIENTO DE RECURSOS SOCIALES PARA MUJERES VÍCTIMAS DE VIOLENCIA DE GÉNERO EN LAS PROVINCIAS DE MOSTAR Y NEVESINJE")
  }
  if(year == 2006) {
    crs = crstemp
  } else{
    namestemp = names(crstemp)
    namescrs = names(crs)
    similarity = namestemp %in% namescrs %>% sum
    similarity2 = namescrs%in% namestemp  %>% sum
    if(similarity == length(namescrs) &
       similarity2 == length(namestemp)) {
      crs = rbind(crstemp, crs)
    } else {
      crs = rbind.fill(crstemp, crs)
      print(paste("Warning for year", year))
    } 
  }
  print(paste(year, "finished"))
  rm(crstemp)
} 
beepr::beep(2)
#save(crs, file="crs2006To2016Raw.Rdata")

crs = crs[!is.na(crs$DonorCode),]
save(crs, file = "crs_in_2020 2.RData")
rm(list = ls()[which(ls()!="crs")])
gc()


