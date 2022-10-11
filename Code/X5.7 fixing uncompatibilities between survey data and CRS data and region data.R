

# This script cannot be run because all variables are not loaded

# df_regions <- df_regions %>%
#   rename(dac_recipientcode  = recipientcode)
# write_rds(df_regions, file = "data/auxiliary/regions.rds")
# notes_regions <- read_rds("data/auxiliary/regions_notes.rds")
## What is the reigon id and region name? I guess it's from DAC codebase


# correct a line in df_regions for south sudan
# also convert all factor cols to chars
# fun_factor2text <- function(x) {
#   if(is.factor(x)) x <- as.character(x)
#   return(x)
# }
# fun_factor2text(df_regions$isocode)=
# df_regions <- lapply(df_regions, fun_factor2text) %>%
#   bind_cols()
# df_regions %>% filter(isocode == "" | is.na(isocode)) %>% as.data.frame()
# df_regions[200,3] = "SSD"
# df_regions[200,6] = countrycode("SSD","iso3c","iso3n")
# df_regions[200,]
# df_regions[df_regions == ""] = NA
# write_rds(df_regions, file = "data/auxiliary/regions.rds")


# find out the recipients without region id matched
df_survey_recipients_regions %>% 
  filter(is.na(regionid)) %>% 
  select(recipientname) %>% 
  unique

df_survey_recipients_regions %>% 
  filter(is.na(regionid)) %>% 
  # select(recipientname) %>% 
  unique %>%
  write_csv("data/Analysis/survey_leftover_recipientnames.csv")
df_regions %>% write_csv("data/Analysis/regions.csv")
# checking what are the empty ones. Are they error from splitting or are they from data? 
a <- df_survey_recipients_regions %>% filter(recipientname == "") %>% .$db_ref
df_survey_recipients_regions %>% filter(db_ref %in% a) %>% select(db_ref) %>% unique
df_survey_recipients_leftovers <- read.csv("Data/Analysis/survey_leftover_recipientnames_fixed.csv", 
                                           encoding = "utf-8") %>%
  rename(recipientname_new = recipientname)
write_rds(df_survey_recipients_leftovers, file = "Data/auxiliary/survey_recipient_names_correction.rds")

#adding multilateral unspecified to the list of regions

df_regions %>% write.csv(file = "Data/analysis/region_template_soWeCanAdd.csv", 
                         row.names = F)
df_regions_complement <- read_csv("data/Analysis/regions_added_202210.csv")
df_regions <-  rbind(df_regions, df_regions_complement) %>% unique
df_regions %>% tail


# fixing the encoding problem in the data 
# this may require second check up
df_regions <- df_regions %>% arrange(recipientname)
df_regions$recipientname[31] <- "Côte d'Ivoire"
df_regions$recipientname[138] <- "Nord du Sahara, régional"

# now creating some schemes to fix the issues in multple-region scenario under on projet
regions2regions_larger <- read_csv("data/Analysis/regions_larger.csv")
df_regions <- df_regions %>% 
  select(-regionname_larger, -regionid_larger) %>% 
  inner_join(regions2regions_larger) 

write_rds(df_regions, "Data/auxiliary/regions.rds")

vec_notes_regions <- read_rds("Data/auxiliary/regions_notes.rds")
vec_notes_regions <- c(vec_notes_regions, "Multilateral_unspecified is added to the region list oin October 2022", "regionname_larger is only used to unify the multiple regions presented under a project") %>% unique
write_rds(notes_regions, "data/auxiliary/regions_notes.rds")

