## 2.2 reproduce master sheet


e_output$master <- df_crs_d4d %>% 
  filter(DonorName == var_donor_working)
# Add a new sheet and write data
addWorksheet(wb, "master")
writeData(wb, "master",e_output$master)


## 2.3 reproduce the review page

e_output$review <- e_output$master %>% 
  filter(Year == max(Year))

addWorksheet(wb, "Review -  2021 D4D Projects ")
writeData(wb, "Review -  2021 D4D Projects ",e_output$review)

## 2.4 reproduce the by year and agency sheet


e_output$by_year <- e_output$master %>% 
  group_by(Year, AgencyName) %>% 
  summarise(cnt = n()) %>% 
  spread(key = AgencyName, value  = cnt)

addWorksheet(wb, "by year and agency")
writeData(wb, "by year and agency",e_output$by_year)

## 2.5 reproduce the by channel and agency sheet

e_output$by_channel <- e_output$master %>% 
  group_by(ChannelName, AgencyName) %>% 
  summarise(cnt = n()) %>% 
  spread(key = AgencyName, value  = cnt) %>% 
  mutate(ChannelName = ifelse(is.na(ChannelName)|ChannelName == "", 
                              "Other", 
                              ChannelName))


addWorksheet(wb, "by channel and agency")
writeData(wb, "by channel and agency",e_output$by_channel)


## 2.6 reproduce the by channel and agency sheet for three recent years

e_output$by_channel_3y <- e_output$master %>% 
  filter(Year > (max(Year)-3)) %>% 
  group_by(ChannelName, AgencyName) %>% 
  summarise(cnt = n()) %>% 
  spread(key = AgencyName, value  = cnt) %>% 
  mutate(ChannelName = ifelse(is.na(ChannelName)|ChannelName == "", 
                              "Other", 
                              ChannelName))


addWorksheet(wb, "by channel and agency 2018-21")
writeData(wb, "by channel and agency 2018-21",e_output$by_channel_3y)

## 2.7 reproduce the by purposecode and agency sheet

e_output$by_ppcode <- e_output$master %>% 
  group_by(PurposeCode, AgencyName) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  arrange(PurposeCode, AgencyName) %>% 
  spread(key = AgencyName, value  = cnt) 


addWorksheet(wb, "by purpose code and agency")
writeData(wb, "by purpose code and agency",e_output$by_ppcode)


# 3 save the workbook

path_output_excel = paste0("output/CH/D4D Validation/", 
                           var_donor_working, 
                           " - D4D Profiles update ", 
                           year(Sys.Date()), 
                           " - review.xlsx")
# Save the workbook
saveWorkbook(wb, path_output_excel, overwrite = TRUE)

# e_output$master %>% write_feather("data/intermediate/13.2 master data of a specific donor.feather")
# df_crs_d4d %>% write_feather("data/intermediate/13.2 master data for D4D.feather")

rm(list = ls(e_output), envir = e_output)

removeWorksheet(wb, "master")
removeWorksheet(wb, "Review -  2021 D4D Projects ")
removeWorksheet(wb, "by year and agency")
removeWorksheet(wb, "by channel and agency")
removeWorksheet(wb, "by channel and agency 2018-21")
removeWorksheet(wb, "by purpose code and agency")
print(i)
