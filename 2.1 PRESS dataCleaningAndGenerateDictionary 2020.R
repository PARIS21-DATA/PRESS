# merge with the PRESS survey data to get more positive samples
##############
# load("crs_2019_clean3.RData")
rm(list = ls())

names(crs)
load("~/dropbox/paris21/press/CRS/press.survey.non0.2019.RData")

press = press.survey.non0

## 2.a. 
press <- press[!press$ReporterId %in% c(0,67),] ## empty entries and PARIS21 test entries
press$id = -1*as.numeric(row.names(press))
press$description <- with(press, paste(ProgramName, Objectives, sep=". "))
press$SCB <- 1
press$POP <- 0
press$projectID <- -1*as.numeric(as.factor(press$description))
press <- press[!duplicated(press$projectID),]
press$title = press$ProgramName
press$descL = press$Objectives
press$stats = 1
names(crs)

press <- press[,c("projectID","SCB","POP","description", "title", "descL", "stats","id")]

press$toDetect = press$descL 
press$toDetect[which(is.na(press$toDetect))] = ""
press$toDetect[press$toDetect == ""] = press$title[press$toDetect == ""]

language1 <- detectLanguage(press$toDetect, isPlainText=TRUE)$detectedLanguage
language2 = cld2::detect_language(press$toDetect)
language3 = cld3::detect_language(press$toDetect)


languagePRESS= data.frame(press$projectID, language1, language2, language3, stringsAsFactors = F) 
head(languagePRESS)
save(languagePRESS, file = "languageDetectionPRESS.RData")
head(press$description)
tail(languagePRESS)
tail(crs$descL)

save(press, file = "press_2020_clean0.5.RData")



load("press_2020_clean0.5.RData")
## 2.b. split projects by sentences
library(reshape2)
library(splitstackshape)
press <- cSplit(press, "description", ".", "long")
press <- cSplit(press, "description", " / ", "long")

## 2.c. replace apostrophes and brackets with blank space in description
# press$description <- gsub("\'", " ", press$description)
# press$description <- gsub("\`", " ", press$description)
# press$description <- gsub("[’]", " ", press$description)
# press$description <- gsub("[(]", " ", press$description)
# press$description <- gsub("[)]", " ", press$description)
# press$description <- gsub("[??]", "é", press$description)
# press$description <- gsub("[?]", " ", press$description)



## 2.d. identify language of project description and drop all except 4 top languages

##############

# library(cldr)
# press$description <- tolower(press$description)
# language <- detectLanguage(press$description, isPlainText=TRUE)$detectedLanguage
# 
# 
# nc <- c("ENGLISH","FRENCH","SPANISH")
# press$language <- with(language,ifelse(detectedLanguage %in% nc, detectedLanguage, "other"))
# rm(language,nc)
############

# table(press$language)
# save(press, file = "press_2019.clean3.RData")


################

names(crs)
names(press)
crs <- rbind.fill(crs, press)
# table(crs$language)
rm(press)
head(crs$description)
head(crs$toDetect)
gc()
save(crs, file="crs_combined_2020.RData")
beep(4)
