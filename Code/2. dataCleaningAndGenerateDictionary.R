library(dplyr)
library(reshape2)
library(splitstackshape)
# setwd("/users/yutian/downloads/crs")
setwd("~/Dropbox/PARIS21/PRESS/PRESS 2020/Analysis")

load("crs_in_2020.Rdata")


names(crs)<- tolower(names(crs))
crs$id = row.names(crs)
## combine projecttitle, shortdescription and longdescription
crs$description <- with(crs, paste(projecttitle, shortdescription, longdescription, sep=". "))

## create project ID based on donorcode and unique project desciption
crs$description.200 <- paste(crs$donorcode, crs$description,sep="_")
crs$projectID <- as.numeric(as.factor(crs$description.200))
## add SCB identifier
crs$SCB <- ifelse(crs$purposecode==16062,1,0)
crs$POP <- ifelse(crs$purposecode==13010,1,0)
crs <- crs[,c("id","projectID","SCB","POP","description")]

#Output::
save(crs, file="crs_2020_clean.RData")

load("crs_2020_clean.RData")
# this is to make the project with same description as 1 as long as one of the same description is marked as 1
crs <- crs %>% group_by(projectID) %>% mutate(SCB=ifelse(
  sum(SCB>0), rep(1,length(SCB)) ,rep(0,length(SCB))
))
crs <- crs %>% group_by(projectID) %>% mutate(POP=ifelse(
  sum(POP>0), rep(1,length(POP)) ,rep(0,length(POP))
))
crs <- group_by(crs, projectID)
## 1.b. drop duplicate project descriptions 
crs <- crs[!duplicated(crs[,"projectID"]),]
dim(crs)
table(crs$SCB)
## 1.c. split projects by language delimiters "." and " / "

crs <- as.data.frame(crs)
library(cldr)
crs$description <- tolower(crs$description)
language <- detectLanguage(crs$description, isPlainText=TRUE)
table(language)
a = language$detectedLanguage
tail(a)
crs <- cSplit(crs, "description", ".", "long")
crs <- cSplit(crs, "description", " / ", "long")
save(crs, file="crs_2019_clean1.RData")






load("crs_2019_clean1.RData")

crs$description <- tolower(crs$description)
language <- detectLanguage(crs$description, isPlainText=TRUE)
nc <- c("ENGLISH","FRENCH","SPANISH")
crs$language <- with(language,ifelse(detectedLanguage %in% nc, detectedLanguage, "other"))
rm(language,nc)
table(crs$language)
save(crs, file="crs_2019_clean2.RData")


crs$description = iconv(crs$description, 'utf-8', 'ascii', sub=' ')
#Output::
save(crs, file="crs_2019_clean3.RData")






rm(list = ls())
gc()

load("crs_2019_clean3.RData")
load("/Users/yutian/Dropbox/PRESS2018/press.survey.non0.2019.RData")

press = press.survey.non0
## 2.a. 
press <- press[!press$ReporterId %in% c(0,67),] ## empty entries and PARIS21 test entries
press$description <- with(press, paste(ProgramName, Objectives, sep=". "))
press$SCB <- 1
press$POP <- 0
press$projectID <- -1*as.numeric(as.factor(press$description))
press <- press[!duplicated(press$projectID),]
press <- press[,c("projectID","SCB","POP","description")]

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
library(cldr)
press$description <- tolower(press$description)
language <- detectLanguage(press$description, isPlainText=TRUE)
nc <- c("ENGLISH","FRENCH","SPANISH")
press$language <- with(language,ifelse(detectedLanguage %in% nc, detectedLanguage, "other"))
rm(language,nc)
table(press$language)
save(press, file = "press_2019.clean3.RData")


crs <- rbind(crs, press)
table(crs$language)
rm(press)

save(crs, file="crs_combined_2019.RData")




rm(list = ls())
load("crs_combined_2019.Rdata")
source("functions.R")

## 3.a. choose language
lang <- "ENGLISH" 
# lang <- "FRENCH"
# lang <- "SPANISH"
language <- tolower(lang)
crs.lang.1 <- crs[crs$language==lang & crs$SCB==1,]
crs.lang.0 <- crs[crs$language==lang & crs$SCB==0,]

## 3.b. merge back by projectID to reverse previous split of project by language
library(dplyr)  
crs.lang.1 <- data.frame(crs.lang.1 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))
crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))
# 
# Warning messages:
#   1: In get(object, envir = currentEnv, inherits = TRUE) :
#   restarting interrupted promise evaluation
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation

## (3.b. english language only)
crs.lang.0.p2 <- crs.lang.0[1:279858,]
crs.lang.0.p1 <- crs.lang.0[279859:559716,]
crs.lang.0.p3 <- crs.lang.0[559717:nrow(crs.lang.0),]

## 3.c. Pre-process documents
## es=2, fr=5, en=40
Min.1 <- 30/nrow(crs.lang.1) ## only consider words that are in >5 SCB projects
crs.dtm.1 <- preprocessing(crs.lang.1$description, language=language)
crs.dtm.1 <- DTM(crs.dtm.1, Min=Min.1, Max=1)
myDict <- crs.dtm.1$dimnames$Terms


## (3.c. english language only)
save(crs.dtm.1, file = "crs.dtm.1_en_2019.RData")

crs.dtm.0.p2 <- preprocessing(crs.lang.0.p2$description, language=language)
crs.dtm.0.p1 <- preprocessing(crs.lang.0.p1$description, language=language)
crs.dtm.0.p3 <- preprocessing(crs.lang.0.p3$description, language=language)

save(crs.dtm.0.p2, file="crs_preprocessed_en.p2_2019.RData")
save(crs.dtm.0.p1, file="crs_preprocessed_en.p1_2019.RData")
save(crs.dtm.0.p3, file="crs_preprocessed_en.p3_2019.RData")

## (3.d. english language only)
crs.dtm.0.p2 <- DTM(crs.dtm.0.p2, dict=myDict)
crs.dtm.0.p1 <- DTM(crs.dtm.0.p1, dict=myDict)
crs.dtm.0.p3 <- DTM(crs.dtm.0.p3, dict=myDict)
crs.dtm.0 <- rbind(crs.dtm.0.p2, crs.dtm.0.p1, crs.dtm.0.p3)
rm(crs.dtm.0.p1, crs.dtm.0.p2, crs.dtm.0.p3, crs.lang.0.p1, crs.lang.0.p2, crs.lang.0.p3)
save(crs.dtm.0, file="crs_myDict_en_2019.RData")
load("crs_myDict_en_2019.RData")

dim(crs.dtm.1)
dim(crs.dtm.0)






lang <- "FRENCH"
language <- tolower(lang)
crs.lang.1 <- crs[crs$language==lang & crs$SCB==1,]
crs.lang.0 <- crs[crs$language==lang & crs$SCB==0,]

## 3.b. merge back by projectID to reverse previous split of project by language
library(dplyr)  
crs.lang.1 <- data.frame(crs.lang.1 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))
crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))

## (3.c. french and spanish only)
## 3.c. Pre-process documents
## es=2, fr=5, en=40
Min.1 <- 3/nrow(crs.lang.1) ## only consider words that are in >5 SCB projects
crs.dtm.1 <- preprocessing(crs.lang.1$description, language=language)
crs.dtm.1 <- DTM(crs.dtm.1, Min=Min.1, Max=1)
myDict <- crs.dtm.1$dimnames$Terms

crs.dtm.0 <- preprocessing(crs.lang.0$description, language=language)
save(crs.dtm.0, file=paste("crs_preprocessed_",substr(language,1,2),"_2019.RData",sep=""))
save(crs.dtm.1, file=paste("crs.dtm.1",substr(language,1,2),"_2019.RData",sep=""))



## 3.d. create Document-Term-Matrix based on myDict
crs.dtm.0 <- DTM(crs.dtm.0, dict=myDict)
save(crs.dtm.0, file=paste("crs_myDict_",substr(language,1,2),"_2019.RData",sep=""))
load(paste("crs_myDict_",substr(language,1,2),"_2019.RData",sep=""))


#######################
#SPANISH
######################
lang <- "SPANISH"
language <- tolower(lang)
crs.lang.1 <- crs[crs$language==lang & crs$SCB==1,]
crs.lang.0 <- crs[crs$language==lang & crs$SCB==0,]

## 3.b. merge back by projectID to reverse previous split of project by language
library(dplyr)  
crs.lang.1 <- data.frame(crs.lang.1 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))
crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% summarise(
  description=paste(description, collapse=". ")))

## (3.c. french and spanish only)
## 3.c. Pre-process documents
## es=2, fr=5, en=40
Min.1 <- 1/nrow(crs.lang.1) ## only consider words that are in >5 SCB projects
crs.dtm.1 <- preprocessing(crs.lang.1$description, language=language)
crs.dtm.1 <- DTM(crs.dtm.1, Min=Min.1, Max=1)
myDict <- crs.dtm.1$dimnames$Terms
crs.dtm.0 <- preprocessing(crs.lang.0$description, language=language)
save(crs.dtm.0, file=paste("crs_preprocessed_",substr(language,1,2),"_2019.RData",sep=""))
save(crs.dtm.1, file=paste("crs.dtm.1",substr(language,1,2),"_2019.RData",sep=""))

## 3.d. create Document-Term-Matrix based on myDict
crs.dtm.0 <- DTM(crs.dtm.0, dict=myDict)
save(crs.dtm.0, file=paste("crs_myDict_",substr(language,1,2),"_2019.RData",sep=""))
load(paste("crs_myDict_",substr(language,1,2),"_2019.RData",sep=""))







#####################################################################
## what is this???? ##


## 3.e. odds ratios for shortlists
library(slam)
freq.1 <- col_sums(crs.dtm.1)/nrow(crs.lang.1)
freq.0 <- col_sums(crs.dtm.0)/nrow(crs.lang.0)
odds <- freq.1/freq.0
sort(odds[odds<Inf & odds>5],decreasing=TRUE)

crs.lang.1 <- data.frame(crs.lang.1)
crs.lang.1[as.data.frame(as.matrix(crs.dtm.1))[,"sampl"]==1,"description"]
crs.lang.0 <- data.frame(crs.lang.0)
crs.lang.0[as.data.frame(as.matrix(crs.dtm.0))[,"sampl"]==1,"description"]

library(beepr)
beep()
#Output::
#save(crs.dtm, file="crs.dtm_wide.RData")





