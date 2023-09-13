source("code/00. boot.R")
rm(list = ls())
path_input <- "data/intermediate/07.3 d4d analysis.feather"
path_output <- "data/intermediate/07.3b d4d manual blacklist.feather"

df <- read_feather(path_input)

df <- df %>% 
  mutate(other_stats = 1)

df <- df %>%
  # Belgium
  mutate(other_stats = if_else(crsid == "20180C2918", 0, other_stats)) %>%
  # CaDB
  mutate(other_stats = if_else(str_detect(projecttitle, "contamination survey") & purposecode != 16062, 0, other_stats)) %>%
  # Czech Republic
  mutate(other_stats = if_else(str_detect(projecttitle, "hydrogeological survey") & purposecode != 16062, 0, other_stats)) %>%
  
  # Denmark 
  mutate(other_stats = if_else(str_detect(projecttitle, "fiber optic backbone link"), 0, other_stats)) %>%
  
  # France
  mutate(other_stats = case_when(
    str_detect(projecttitle, "fasep 1096-creation de data centers souverains") ~ 0,
    str_detect(projecttitle, "soutien de la politique de l'éducation nationale") ~ 0,
    str_detect(projecttitle, "soutien aux politiques d'éducation") ~ 0,
    str_detect(projecttitle, "soutienpolitiqueeduc") ~ 0,
    str_detect(projecttitle, "enquete qualitative") ~ 0,
    str_detect(projecttitle, "sediment survey") ~ 0,
    TRUE ~ other_stats
  ))



df <- df %>%
  # Germany
  mutate(other_stats = if_else(
    str_detect(projecttitle, "database to document human rights violations") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "wreck management information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "patent information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(longdescription, "support the implementation of environmental legislation in particular as regards compliance or restoration of forest policy for private ownership of land") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Italy
  mutate(other_stats = if_else(
    str_detect(projecttitle, "penitentiary information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "geophisical survey of the valley of the kings") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "metallic artefacts") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "philosophy / data sciences") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "increasing the number of international students attending phd courses in: data science") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "database for election observation missions") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Drop specific rows
  filter(!(crsid == "2017000137" & purposecode == 16062 & donorname == "Italy"))



library(dplyr)
library(stringr)

df <- df %>%
  
  # Luxembourg
  mutate(other_stats = if_else(
    str_detect(projecttitle, "minusma: commission d'enquête internationale") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Netherlands
  mutate(other_stats = if_else(
    str_detect(projecttitle, "survey on nl & id history") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "cot_port_cotonou_quai_survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "court decision database") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "ilo rmg program"),
    0, other_stats
  )) %>%
  
  # Poland
  mutate(other_stats = if_else(
    channelreportedname == "national bank of poland",
    0, other_stats
  )) %>%
  
  # Sweden
  mutate(other_stats = if_else(
    str_detect(projecttitle, "surveying political candidates"),
    0, other_stats
  )) %>%
  
  # Switzerland
  mutate(other_stats = if_else(
    str_detect(projecttitle, "small arms survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "mine survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # UK
  mutate(other_stats = if_else(
    str_detect(projecttitle, "national accountability") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "international account") & purposecode != 16062,
    0, other_stats
  ))



library(dplyr)
library(stringr)

df <- df %>%
  
  # US
  mutate(other_stats = if_else(
    str_detect(projecttitle, "geological survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "remnants survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "explosive remnants") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "cluster munition and other conventional weapons survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "unexploded ordnance") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "explosive ordnance") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "clearance") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "demining") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "mine action") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "demining") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Denmark
  mutate(other_stats = if_else(
    str_detect(projecttitle, "hydrogeological survey") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Japan
  mutate(other_stats = if_else(
    str_detect(projecttitle, "risques des mines") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "disaster management information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Korea
  mutate(other_stats = if_else(
    str_detect(projecttitle, "mine hazard") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "law information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "emergency management information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "public safety management information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "public saftey management information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "immigration information system") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # EU
  mutate(other_stats = if_else(
    str_detect(projecttitle, "survey of hydrogeological") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "catastrophe") & purposecode != 16062,
    0, other_stats
  )) %>%
  
  # Spain
  mutate(other_stats = if_else(
    str_detect(projecttitle, "catastrophic") & purposecode != 16062,
    0, other_stats
  ))

library(dplyr)
library(stringr)

df <- df %>%
  
  # Switzerland
  mutate(other_stats = if_else(
    str_detect(projecttitle, "demining") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    str_detect(projecttitle, "mine victims") & purposecode != 16062,
    0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020003921" & 
      str_detect(projecttitle, "financement enquête et plaidoyer sur l'exploitation de la forêt en amazonie") & 
      donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2018005231" & 
      str_detect(projecttitle, "medici senza frontiere - sostegno per interventi in caso di catastrofe") & 
      donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020006913" & 
      str_detect(projecttitle, "mission d'enquête sur les résultats des projets au kwilu") & 
      donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020008519" & 
      str_detect(projecttitle, "survey comprehensive sexuality education - small action credit") & 
      donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020006926" & 
      str_detect(projecttitle, "enquête conjointe et plaidoyer auprès des nations unies sur l'impact négatif des mégaprojets au guatemala") & 
      donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020008679" & 
      str_detect(projecttitle, "fao/pam: enquête sécurité alimentaire - small action credit") & 
      purposecode == 31110 & donorname == "Switzerland", 0, other_stats
  )) %>%
  mutate(other_stats = if_else(
    crsid == "2020008679" & 
      str_detect(projecttitle, "fao/pam: enquête sécurité alimentaire - small action credit") & 
      purposecode == 43071 & donorname == "Switzerland", 0, other_stats
  )) %>%
  
  # United Arab Emirates
  mutate(other_stats = if_else(
    str_detect(projecttitle, "information system related to the comp") & purposecode != 16062,
    0, other_stats
  )) %>%
  # Individual Projects
  mutate(other_stats = if_else(crsid %in% c(
    "2011000041", "2016000008", "2010000119", "2011000448", "2012000002",
    "2013021796", "2012000094", "2012000093", "2011000180", "2013000005"), 0, other_stats
  )) %>%
  # Exclusion based on purpose codes
  mutate(other_stats = if_else(purposecode == 15250, 0, other_stats))


df <- df %>% 
  select(db_ref, other_stats) 

df <- df %>% 
  rename(black_list = other_stats) %>% 
  mutate(black_list = ifelse(black_list == 1, 0, 1)) %>% 
  filter(black_list == 1)

write_feather(df, path_output)



