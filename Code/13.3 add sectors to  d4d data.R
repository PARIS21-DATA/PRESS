#########################################
### first layer: define class of statistic by sector, assigning all activities under purpose code 16062 initially to general statistical capacity building
#########################################

df_crs_d4d <- df_crs %>% 
  mutate(crsid = db_original_id) %>% 
  mutate(donorname = dac_donorname)
# Add a new variable 'sector' and initialize with NA
df_crs_d4d$sector <- NA

# Define labels for 'sector'
sector_labels <- c(
  "1" = "General statistical capacity",
  "2" = "Population",
  "3" = "Education",
  "4" = "Health",
  "5" = "Economy",
  "6" = "Agriculture",
  "7" = "Gender",
  "8" = "Environment",
  "9" = "Other"
)

# Define class of statistic by sector
df_crs_d4d$sector[df_crs_d4d$purposecode == 16062] <- 1
df_crs_d4d$sector[between(df_crs_d4d$purposecode, 13000, 13999)] <- 2
df_crs_d4d$sector[between(df_crs_d4d$purposecode, 11000, 11999)] <- 3
df_crs_d4d$sector[between(df_crs_d4d$purposecode, 12000, 12999)] <- 4

econ_codes <- c(15123, 15124, 15196, 15111, 15117, 15118, 15119, 
                15114, 15116, 15155, 15156, 15125)
df_crs_d4d$sector[df_crs_d4d$purposecode %in% econ_codes] <- 5
df_crs_d4d$sector[df_crs_d4d$purposecode == 15142 |
              between(df_crs_d4d$purposecode, 23000, 25999) |
              between(df_crs_d4d$purposecode, 32000, 32999) |
              between(df_crs_d4d$purposecode, 33000, 33999) |
              between(df_crs_d4d$purposecode, 21000, 21999)] <- 5
df_crs_d4d$sector[between(df_crs_d4d$purposecode, 31000, 31999) |
              between(df_crs_d4d$purposecode, 43071, 43073)] <- 6
df_crs_d4d$sector[between(df_crs_d4d$purposecode, 15170, 15180)] <- 7
df_crs_d4d$sector[df_crs_d4d$purposecode == 14050 |
              df_crs_d4d$purposecode == 14015 |
              between(df_crs_d4d$purposecode, 41000, 41999) |
              between(df_crs_d4d$purposecode, 43040, 43049)] <- 8
df_crs_d4d$sector[is.na(df_crs_d4d$sector)] <- 9
df_crs_d4d$sector[grepl("aiddata center", df_crs_d4d$projecttitle, ignore.case = TRUE) |
              grepl("construction of data centers for national id system project",
                    df_crs_d4d$projecttitle, ignore.case = TRUE)] <- 9

# Add an additional variable 'sector1' as a copy of 'sector' 
df_crs_d4d$sector1 <- df_crs_d4d$sector

# Adding the sector labels (optional, useful for plots and tables)
df_crs_d4d$sector_label <- factor(df_crs_d4d$sector, levels = names(sector_labels), labels = sector_labels)

rm(sector_labels)

#########################################
### second layer: based on text mining of project titles
#########################################

df_crs_d4d <- df_crs_d4d %>%
  mutate(
    sector = case_when(
      # Second layer based on text mining of project titles
      stri_detect_fixed(projecttitle, "central bureau of statistics") & 
        stri_detect_fixed(projecttitle, "core funding") ~ 1,
      crsid == "2016001694" ~ 2,
      stri_detect_regex(projecttitle, "population census|housing census|crvs|birth registr|civil registr|migration data|migration statistics|world bank/unhcr joint data center|support to the identification of poor households \\(idpoor\\) programme") ~ 2,
      stri_detect_regex(projecttitle, "education management information system|emis") ~ 3,
      stri_detect_fixed(projecttitle, "global programme digital transformation") & purposecode == 22040 & donorname == "Germany" ~ 3,
      stri_detect_regex(projecttitle, "health management information system|health information system|hmis|demographic and health survey|dhs|washington group|disability data|death registr") ~ 4,
      stri_detect_fixed(projecttitle, "social protection innovation and learning") & purposecode == 16010 & donorname == "Germany" ~ 4,
      stri_detect_regex(projecttitle, "economic statistic|fiscal statistic|finance statistic|business registr|national accounts|tax revenue information system|land registr|land valuation|business ownership survey|business survey|enterprise survey|enterprise statist|business census|enterprise census|price statistic|consumer price|producer statistic|trade statistic|international comparison program") ~ 5,
      stri_detect_fixed(projecttitle, "programme macroeconomic reform - green growth") & purposecode == 15110 & crsid == "2020002030a" & donorname == "Germany" ~ 5,
      stri_detect_regex(projecttitle, "agricultural census|agricultural survey|farm survey") ~ 6,
      stri_detect_fixed(projecttitle, "2030") & stri_detect_fixed(projecttitle, "50") ~ 6,
      stri_detect_regex(projecttitle, "gender statist|statistics for gender equality") ~ 7,
      stri_detect_regex(projecttitle, "environment statist|environmental statist|geographic information system|climate information system|ocean observation|biodiversity|forest survey|plant genetic resources|land use|land cover|land information system|meteorological satellite data") ~ 8,
      stri_detect_fixed(projecttitle, "glacier monitoring for energy and water security") ~ 8,
      stri_detect_fixed(projecttitle, "implementation of 2030 agenda in bolivia") & purposecode == 14010 & donorname == "Germany" ~ 8,
      TRUE ~ sector  # Retain the original sector value for all other cases
    ),
    sector2 = sector  # Keeping track of how layers affect allocation
  )


#########################################
### third layer: based on main purpose of multilateral through which aid is delivered
#########################################



df_crs_d4d <- df_crs_d4d %>%
  mutate(
    sector = case_when(
      (channelcode == 47066 | channelcode == 41121 | channelcode == 41119) & (sector == 9 | sector == 1) ~ 2,
      (channelcode == 41304 | channelcode == 47501) & (sector == 9 | sector == 1) ~ 3,
      (channelcode == 41110 | channelcode == 41307 | channelcode == 41143 | channelcode == 47122 | channelcode == 47045 | channelcode == 47083) & (sector == 9 | sector == 1) ~ 4,
      (between(channelcode, 43000, 43999) | channelcode == 41123) & (sector == 9 | sector == 1) ~ 5,
      (channelcode == 41301 | channelcode == 41108 | channelcode == 47063) & (sector == 9 | sector == 1) ~ 6,
      channelcode == 41146 & (sector == 9 | sector == 1) ~ 7,
      (channelcode == 47129 | channelcode == 47130 | channelcode == 47044 | channelcode == 41317 | channelcode == 47067 | channelcode == 41116 | channelcode == 41316) & (sector == 9 | sector == 1) ~ 8,
      TRUE ~ sector  # Retain the original sector value for all other cases
    )
  )

#########################################
### fourth layer: additional work
#########################################


df_crs_d4d <- df_crs_d4d %>%
  # Keeping track on how layers affect allocation
  mutate(sector3 = sector) %>%
  
  # Manual curation
  mutate(
    sector = case_when(
      str_detect(longdescription, "support for the population and housing census") ~ 2,
      str_detect(projecttitle, "unfpa, census 2012") ~ 2,
      TRUE ~ sector
    )
  ) %>%
  
  # New purpose codes
  mutate(
    sector = case_when(
      purposecode == 12196 ~ 4,
      purposecode == 13096 ~ 2,
      TRUE ~ sector
    )
  ) 


## some parent channel codes are missing

df_crs %>% 
  select(parentchannelcode, channelcode) %>% 
  distinct() %>% 
  arrange(channelcode, parentchannelcode) %>% 
  filter(duplicated(channelcode))

df_crs_d4d <- df_crs_d4d %>%
  # Other manipulation
  mutate(parentchannelcode = ifelse(is.na(parentchannelcode), 
                                    0, 
                                    parentchannelcode)) %>% 
  mutate(
    parentchannelcode = case_when(
      # is.na(parentchannelcode) ~ 0, 
      # parentchannelcode == 51000 ~ 51000,
      (parentchannelcode == 51000 &
        str_detect(channelreportedname, "ssb -") &
        donorname == "Norway" ) ~ 11000,
      ((parentchannelcode == 51000) &
        (str_detect(projecttitle, "pcbs") | str_detect(channelreportedname, "pcbs"))  &
        (donorname == "Norway")
       ) ~ 12000,
      TRUE ~ parentchannelcode
    )
  ) %>%
  # Drop specific rows based on conditions
  filter(
    !purposecode %in% c(91010, 99820) & 
      !between(purposecode, 60000, 69999) &
      flowname != "Equity Investment"
  )





