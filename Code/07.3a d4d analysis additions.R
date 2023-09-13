rm(list = ls())
source("code/00. boot.R")
start_time <- Sys.time()

# df <-readRDS("data/intermediate/crs01_1_full_2023.rds")
# gc()
# names(df)
# df_full <- df
# df <- df %>%
#   select(db_ref,
#          year,
#          longdescription,
#          channelreportedname,
#          purposecode,
#          donorname,
#          agencyname,
#          crsid,
#          projecttitle,
#          usd_disbursement_defl)
# df <- df %>%
#   mutate(across(c("longdescription",
#                   "projecttitle",
#                   "channelreportedname"), ~ tolower(.x)))
# path_donors <- "data/d4d/d4d donors.txt"
# vec_d4d_donors <- readLines(path_donors)
# df <- df %>% 
#   filter(donorname %in% vec_d4d_donors)
# df %>% select(donorname) %>% distinct
# write_feather(df,  "data/intermediate/07.3 d4d analysis.feather")
# print_time_diff(start_time)
# rm(df_full)

path_input <- "data/intermediate/07.3 d4d analysis.feather"
path_output <- "data/intermediate/07.3a d4d manual additions.feather"


df <- read_feather(path_input)

df <- df %>% mutate(other_stats = 0)

# Netherlands and BMGF support the IMF's financial access survey (fas)
df <- df %>% mutate(other_stats = ifelse(str_detect(longdescription, "financial access survey") & purposecode != 16062, 1, other_stats))

# NSOs as implementers
search_terms <- c("statist", "institute of statistics", "mathematical and statistical modelling", 
                  "statistics canada", "fiscal and monetary statistics", "statistical production of ine",
                  "danmarks statistik", "istat")

d4d_donors <- c()

for(term in search_terms) {
  df <- df %>% mutate(other_stats = ifelse(str_detect(channelreportedname, term) & purposecode != 16062, 1, other_stats))
}

df %>% 
  filter(other_stats ==1) %>%
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#donorname,
         # projecttitle, usd_disbursement_defl, 
         channelreportedname)

df <- df %>% mutate(other_stats = ifelse(str_detect(channelreportedname, "istat") & purposecode != 16062 & donorname == "Italy", 1, other_stats))

# MCC
df <- df %>% mutate(other_stats = ifelse(agencyname == "Millennium Challenge Corporation" & 
                                           (str_detect(longdescription, "data-driven communities activity") |
                                              str_detect(longdescription, "data analytics") |
                                              str_detect(longdescription, "data for youth")), 1, other_stats))

# Germany
search_terms_germany <- c("remote sensing data", "information system supplying hydrological and meteorological data", 
                          "capacity development in gis and data base management", "higher resolution data",
                          "data collection and analysis", "data journalism", "processing and evaluation of data",
                          "global forest survey", "consumer data protection", "web-based data analysis platform")

for(term in search_terms_germany) {
  df <- df %>% mutate(other_stats = ifelse(str_detect(longdescription, term) & purposecode != 16062 & donorname == "Germany", 1, other_stats))
}

# Additional projects as asked by Germany
search_terms_project_germany <- c("strengthening good governance kenia", "social protection innovation and learning",
                                  "citizens engagement and innovative data use for africa's development", "partners for review",
                                  "knowledge for nutrition", "soil information for sustainable land use in cameroon",
                                  "support to the moldovan government for the implementation of the 2030 agenda",
                                  "implementation of 2030 agenda in bolivia", "sdg initative namibia", "support to the identification of poor households (idpoor) programme",
                                  "studies and experts fund kyrgyzstan", "programme macroeconomic reform - green growth")

for(term in search_terms_project_germany) {
  df <- df %>% mutate(other_stats = ifelse(str_detect(projecttitle, term) & donorname == "Germany", 1, other_stats))
}

# Additional projects identified by Germany from PARIS21 methodology
crsid_values <- list(c("2010003915", "2010009939", "2010009940"), 
                     c("2010011436", "2010009779"),
                     c("2010009784", "2010009786", "2010009882", "2010010622", "2010011457"),
                     c("2011008444", "2011011550", "2011012301"), 
                     c("2012004669", "2013002091", "2013004973", "2013007894", "2013009143"),
                     c("2014011726", "2015127466", "2016012607", "2016005772", "2017008275"),
                     c("2017002362", "2017004638", "2017011443", "2015127466", "2018003466"),
                     c("2018007639", "2018001985"), 
                     c("2015127466", "2015127466", "2019003811", "2015127466"))
crsid_values <- unlist(crsid_values)

for(values in crsid_values) {
  df <- df %>% mutate(other_stats = ifelse(donorname == "Germany" & crsid %in% values, 1, other_stats))
}

# Projects to be deleted as per exchange with Germany
df <- df %>% filter(!(str_detect(projecttitle, "for uncensored flow of information on public issues by community based broadcast media in latin america") & donorname == "Germany"))

# France
df <- df %>% mutate(other_stats = ifelse(donorname == "France" & 
                                           (str_detect(projecttitle, "financement opal")|
                                              str_detect(projecttitle, "installation de deux stations hydrométriques")) & 
                                           purposecode != 16062, 1, other_stats))

# Switzerland
crsid_switzerland <- c("2016003165", "2016003166", "2016003339", "2016003340", "2016003341")
df <- df %>% mutate(other_stats = ifelse(donorname == "Switzerland" & crsid %in% crsid_switzerland, 1, other_stats))

print_time_diff(start_time)

beepr::beep()

df %>% 
  filter(other_stats ==1) %>%
  arrange(desc(usd_disbursement_defl)) %>% 
  select(donorname, projecttitle, usd_disbursement_defl)

df %>% 
  filter(other_stats == 1) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df <- df %>% 
  filter(other_stats ==1)

df %>%
  select(db_ref, other_stats) %>%
  write_feather(path_output)

