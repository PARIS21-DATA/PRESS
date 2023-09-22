rm(list =ls())
source("code/00. boot.R")
start_time <- Sys.time()
# 1. set up parameters and constant variables
path_input_sdg_markers <- paste0("data/intermediate/09.1 sdg markers UNIQUE goals ", 
                                 year(Sys.Date()), 
                                 ".feather")
path_output_just_climate_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " climate data projects 2018-2021",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_just_climate_all_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " climate data projects 2010-2021",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_press_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " press 2023 data full",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_RDS <- paste0("output/press/", 
                                        Sys.Date(),
                                        " PRESS data with climate markers",
                                        year(Sys.Date()),
                                        ".RDS")

# 2. load data
df_crs <- read_feather("output/ch/current PRESS 2023 data.feather")

df_climate_filters_agency <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                       sheet = 1) %>% 
  filter(Climate == 1)

df_climate_filters_donor <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                      sheet = 2) 

df_climate_filters_channel <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                      sheet = 3) %>% 
  filter(Climate == 1)

df_sdgs <- read_feather(path_input_sdg_markers)
rm(path_input_sdg_markers)

# 3. adapt and implify data to be ready for analysis 
names(df_climate_filters_agency) <- tolower(names(df_climate_filters_agency))
names(df_climate_filters_channel) <- tolower(names(df_climate_filters_channel))
names(df_climate_filters_donor) <- tolower(names(df_climate_filters_donor))

df_climate_filters_agency <- df_climate_filters_agency %>% 
  select(dac_donorcode = donorcode, 
         # dac_donorname = donornamee, 
         agencycode, 
         # agencynamee, 
         climate_agency = climate) %>% 
  mutate(climate_agency = climate_agency == 1)

df_climate_filters_channel <- df_climate_filters_channel %>% 
  select(channelcode = channel.id, 
         climate_channel = climate) %>% 
  mutate(climate_channel = climate_channel == 1)

df_climate_filters_donor <- df_climate_filters_donor %>% 
  select(dac_donorcode = donor.code
         # ,dac_donorname = `donor.name.(en)`
         ) %>% 
  mutate(climate_donor = T)


# 4 analysis 
# 4.1 merge filter data with the main dataset

df_crs <- df_crs %>% 
  left_join(df_climate_filters_agency) %>% 
  left_join(df_climate_filters_channel) %>% 
  left_join(df_climate_filters_donor)

rm(df_climate_filters_agency, 
   df_climate_filters_channel, 
   df_climate_filters_donor)

# 4.2 adding other filters

df_crs <- df_crs %>% 
  mutate(climate_marker = climateadaptation == 2|climatemitigation==2) 

vec_only_sdg13 <- df_sdgs %>% 
  filter(sdg_goal == 13) %>% 
  .$db_ref

rm(df_sdgs)

df_crs <- df_crs %>% 
  mutate(climate_sdg_marker = db_ref %in% vec_only_sdg13)
rm(vec_only_sdg13)
# climat|carbon
df_crs <- df_crs %>% 
  mutate(climate_title = grepl("climat|carbon|climát|calentamiento|invernadero|réchauffement|rechauffement|effet de serre|global warm|kohlenstoff|gewächshaus|gewachshaus|klima|global erwärmung|global erwarmung|treibhäus", projecttitle, ignore.case = T))

# 4.3 fill the NAs in the filter columns
df_crs <- df_crs %>% 
  mutate(across(c("climate_donor", 
                  "climate_agency",
                  "climate_channel", 
                  "climate_marker", 
                  "climate_sdg_marker", 
                  "climate_title"), 
                ~ifelse(is.na(.x), F, .x)))

# 4.4 create the unified filter and add identified_by column
df_crs <- df_crs %>% 
  mutate(climate = climate_donor|climate_agency|climate_channel|climate_marker|
           climate_sdg_marker|climate_title)


df_climate_filter_order <- tibble(markers = c("climate_donor", 
                                              "climate_agency",
                                              "climate_marker",
                                              "climate_sdg_marker", 
                                              "climate_channel", 
                                              "climate_title"), 
                                  order = c(1:6))

tmp_df_climate_key_filter <- df_crs %>% 
  filter(climate) %>% 
  select(db_ref, starts_with("climate_")) %>% 
  gather(-db_ref, key  = "markers", value = "value") 

tmp_df_climate_key_filter <- tmp_df_climate_key_filter %>% 
  filter(value) %>% 
  left_join(df_climate_filter_order) %>% 
  arrange(db_ref, order) %>% 
  group_by(db_ref) %>% 
  filter(row_number() == 1) %>% 
  mutate(identified_by_climate = str_replace(markers, "climate_", "")) %>% 
  select(db_ref, identified_by_climate)


df_crs <- df_crs %>% 
  left_join(tmp_df_climate_key_filter) %>% 
  mutate(identified_by_climate = replace_na(identified_by_climate, "not climate"))
rm(df_climate_filter_order, tmp_df_climate_key_filter)

df_crs %>% 
  saveRDS(path_output_RDS)

# 5. calculations

df_crs %>% 
  filter(climate) %>%
  select(identified_by_climate) %>% 
  table


df_crs %>% 
  filter(climate) %>% 
  # filter(year > 2015) %>% 
  group_by(year) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T),
            cnt = n())




df_crs %>% 
  filter(climate) %>% 
  filter(year > 2017) %>% 
  arrange(desc(year), desc(usd_disbursement_defl)) %>% 
  select(projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate) %>% 
  write.xlsx(path_output_just_climate_xlsx)


df_crs %>% 
  select(reported_year = year, 
         projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate, 
         identified_as_data_because = idenfied_by_stat
  ) %>% 
  write.xlsx(path_output_press_xlsx)


df_crs %>% 
  filter(climate) %>% 
  # filter(year > 2017) %>% 
  arrange(desc(year), desc(usd_disbursement_defl)) %>% 
  select(projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate, 
         db_ref) %>% 
  write.xlsx(path_output_just_climate_all_xlsx)
print_time_diff(start_time )


# 6 share data with DI

rm(# path_output_RDS,
   path_output_just_climate_all_xlsx, 
   path_output_press_xlsx, 
   path_output_just_climate_xlsx)

# df_crs <- readRDS(path_output_RDS)

path_output_2share_RDS <- paste0("output/press/", 
                          Sys.Date(),
                          " PRESS data sharing P21-DI ",
                          year(Sys.Date()),
                          ".RDS")

df_crs_raw <- read_feather("data/intermediate/crs05.2_full_2023.feather")

setdiff(names(df_crs_raw), names(df_crs))

setdiff(names(df_crs), names(df_crs_raw))

df_crs_2share <- df_crs %>% 
  select(-regionname) %>%
  rename_with(~ gsub("^dac_", "", .), starts_with("dac_"))


setdiff(names(df_crs_2share), names(df_crs_raw))
setdiff(names(df_crs_raw), names(df_crs_2share))

df_crs_2share <- df_crs_2share %>%
  rename(crsid = db_original_id ) 

df_crs_2share <- df_crs_2share %>%
  select(-all_of(setdiff(names(df_crs_2share), names(df_crs_raw))[1:5]))

setdiff(names(df_crs_2share), names(df_crs_raw))
setdiff(names(df_crs_raw), names(df_crs_2share))

df_crs_2share <- df_crs_2share %>%
  select(-all_of(setdiff(names(df_crs_2share), names(df_crs_raw))[3:4])) %>% 
  rename(infosys_title = infosys) 

setdiff(names(df_crs_2share), names(df_crs_raw))
setdiff(names(df_crs_raw), names(df_crs_2share))

df_crs_2share <- df_crs_2share %>%
  select(-all_of(setdiff(names(df_crs_2share), names(df_crs_raw))[8:9])) 

setdiff(names(df_crs_2share), names(df_crs_raw))
setdiff(names(df_crs_raw), names(df_crs_2share))

names(df_crs_2share)

df_crs_2share <- df_crs_2share %>% 
  select(-all_of(names(df_crs_2share)[2:17]))

names(df_crs_2share)

df_crs_2share <- df_crs_2share %>% 
  select(-all_of(names(df_crs_2share)[3:11]))


df_crs_2share %>% 
  saveRDS(path_output_2share_RDS)


