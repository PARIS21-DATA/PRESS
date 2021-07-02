setwd("~/Dropbox/PARIS21/PRESS/PRESS 2020/Analysis")

rm(list = ls())
gc()
load("crs_combined_2020_withLanguage_converted to ASCII_connected.RData")

## 1. Blacklist: CRS purposecodes

## 2. Whitelists: 1x NSO names; 3x keywords (english, spanish, french) based on odds ratios
source("functions.R")
beep(4)

dictEN <- wlist("en")
dictFR <- wlist("fr")
dictES <- wlist("es")
# cw <- function(x){
#   unlist(
#     lapply(x, function(i){
#       length(strsplit(i$content$content," ")[1])
#     }), use.names=FALSE)
# }
cw <- function(y) {
  unlist(
    lapply(y$content, function(x) length(unlist(strsplit(x, " "))) )
  )
}



## --- 3. Obtain dictionaries ---
# library(dplyr)  

## 3.a. English

## - count number of words per project description
# load("crs_preprocessed_en.p2_2020.RData")
# load("crs_preprocessed_en.p1_2020.RData")
# load("crs_preprocessed_en.p3_2020.RData")
load("crs_preprocessed_en_2020.RData")
load("crs.dtm.1_en.myDict_2020.RData")

dictEN1 = c(dictEN, myDict)
nwords <- c( cw(crs.dtm.0))
nwords %>% head
beep(4)
## - get DTM for dictEN

crs.dtm.0_1<- DTM(crs.dtm.0, dict=dictEN)
beep(4)
crs.dtm.0_2<- DTM(crs.dtm.0, dict=dictEN1)

# crs.dtm.0.p2 <- DTM(crs.dtm.0.p2, dict=dictEN)
# crs.dtm.0.p1 <- DTM(crs.dtm.0.p1, dict=dictEN)
# crs.dtm.0.p3 <- DTM(crs.dtm.0.p3, dict=dictEN)

# crs.dtm.0 <- rbind(crs.dtm.0.p2, crs.dtm.0.p1, crs.dtm.0.p3)
beep(4)
save(crs.dtm.0, file="crs_dictEN_2020.RData")
# rm(crs.dtm.0.p1, crs.dtm.0.p2, crs.dtm.0.p3)
# load("crs_dictEN_2020.RData")

## - determine benchmark for word count from SCB projects
#crs.lang.1 <- crs[crs$language=="ENGLISH" & crs$SCB==1,]
#crs.lang.1 <- data.frame(crs.lang.1 %>% group_by(projectID) %>% summarise(
#  description=paste(description, collapse=". ")))
#crs.dtm.1 <- preprocessing(crs.lang.1$description, language="english")
#nwords <- cw(crs.dtm.1)
#load("crs_dictEN.RData")
#crs.dtm.1 <- DTM(crs.dtm.1, dict=dictEN)
#library(slam)
#plot(row_sums(crs.dtm.1), nwords)
#table(row_sums(crs.dtm.1) > 0)

## - select projectIDs of non-SCB projects were at least every 50st word is in dictEN
crs.lang.0 <- crs[crs$language=="english" & crs$stats==0,]
# crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% dplyr::summarise(
#   description=paste(description, collapse=". ")))

save(crs.lang.0, file = "crslang0EN_2020.RData")
beep(4)
gc()
# library(slam)

load("crs_dictEN_2020.RData")
load("crslang0EN_2020.RData")
      # a = row_sums((crs.dtm.0_1))
      # head(a)
      # a = crs.dtm.0_1$dimnames 
      # head(a)
      # b =a[["Docs"]]
      # c = a[["Terms"]]
      # head(b)
      # tail(b)
      # 
      # b = data.frame(a) 
# projectIDs.EN <- crs.lang.0$projectID[row_sums(crs.dtm.0) > nwords/100]
projectIDs.EN <- crs.lang.0$projectID[row_sums(crs.dtm.0_1) > nwords/100]
projectIDs.EN <- crs.lang.0$projectID[row_sums(crs.dtm.0_2) > nwords/100]
beep(4)
save(projectIDs.EN, file = "projectIDsEN_2020.RData")

## 3.b. French
## - get DTM for dictFR
load("crs_preprocessed_fr_2020.RData")
nwords <- cw(crs.dtm.0)
load("crs.dtm.1_fr.myDict_2020.RData")

dictFR1 = c(dictFR, myDict)
crs.dtm.0 <- DTM(crs.dtm.0, dict=dictFR1)
save(crs.dtm.0, file="crs_dictFR_2020.RData")
# load("crs_dictFR_2020.RData")

## - select projectIDs of non-SCB projects were at least every 50st word is in dictFR
crs.lang.0 <- crs[crs$language=="french" & crs$SCB==0,]
# crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% dplyr::summarise(
#   description=paste(description, collapse=". ")))
save(crs.lang.0, file = "crslang0FR_2020.RData")
# load("crslang0FR_2020.RData")
projectIDs.FR <- crs.lang.0$projectID[row_sums(crs.dtm.0) > nwords/100]
save(projectIDs.FR, file = "projectIDsFR_2020.RData")
## 3.c. Spanish
## - get DTM for dictES
load("crs_preprocessed_sp_2020.RData")
nwords <- cw(crs.dtm.0)
load("crs.dtm.1_es.myDict_2020.RData")

dictES1 = c(dictES, myDict)

crs.dtm.0 <- DTM(crs.dtm.0, dict=dictES1)
save(crs.dtm.0, file="crs_dictES_2020.RData")
# load("crs_dictES_2020.RData")

## - select projectIDs of non-SCB projects were at least every 50st word is in dictES
crs.lang.0 <- crs[crs$language=="spanish" & crs$SCB==0,]
# crs.lang.0 <- data.frame(crs.lang.0 %>% group_by(projectID) %>% dplyr::summarise(
#   description=paste(description, collapse=". ")))
save(crs.lang.0, file="crslang0ES_2020.RData")
# load("crslang0ES_2020.RData")
projectIDs.ES <- crs.lang.0$projectID[row_sums(crs.dtm.0) > nwords/100]
save(projectIDs.ES, file = "projectIDsES_2020.RData")
## 3.c. ...
# load("projectIDsEN_2020.RData")
projectIDs <- unique(c(projectIDs.EN, projectIDs.FR, projectIDs.ES))

save(projectIDs , file='projectIDs2020.RData')

length(projectIDs)


## 3.d. load fresh crs data and genereta projectID (as in step 'CRS Data Preparation') 
##      to select from projectIDs

rm(list = ls())
load("crs_in_2020.RData")
names(crs) <- tolower(names(crs))
crso <- crs
load("crs_2020_clean.RData")
load("projectIDs2020.RData")
gc()
head(crs)
head(crso)
crso$projectID <- crs$projectID
crs <- crso
rm(crso)
# save(crs, file = "crs_prefiltered_spaceSep.RData")
# load("crs_prefiltered_spaceSep.RData")
# blacklist <- read.csv("data/blacklist.csv", stringsAsFactors=FALSE)
# blacklist <- c(na.omit(with(blacklist, CRS.CODE[blacklist==1])))

# crs <- crs[(crs$projectID %in% projectIDs | crs$purposecode==16062) & 
#              crs$purposecode %in% blacklist, ]

crso = crs

load("crs_combined_2020_withLanguage_converted to ASCII_connected.RData")
projectIDs2 = crs %>%
  filter(stats ==1) %>%
  select(projectID)
projectIDs2 = projectIDs2$projectID %>% unique

crs$source = 0
crs$source[which(crs$projectID %in% projectIDs2)] = 2

crs$source[which(crs$projectID %in% projectIDs | crs$purposecode == 16062)] = 1

table(crs$source)

crs <- crs[(crs$projectID %in% projectIDs | crs$purposecode==16062), ]
dim(crs)
table(crs$year)
#Output::
save(crs, file="crs_filtered_2020.RData")


unique(crs$donorname)
