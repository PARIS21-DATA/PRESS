
# filter(text_detection_wo_infoSys)
# new_reporter= data.frame(ReporterId = NA, ReporterName = NA, donorname = "Bloomberg Family Foundation", press = 0, crs = 1, donorcode = 1640, donor_type = "private", donorname_unified = "Bloomberg Family Foundation" )
# reporters = rbind(reporters, new_reporter) 
# save(reporters, file = "./analysis/reporters_2021.rdata")
# rm(new_reporter)



# crspress = crspress[,order(names(crspress))]
# press = press[,order(names(press))]
# 
# names(press)
# names(crspress)

# crspress$donorname[which(!(crspress$donorcode %in% reporters$donorcode))]
# crspress$donorcode[which(!(crspress$donorcode %in% reporters$donorcode))]
# which(!(crspress$donorcode %in% reporters$donorcode))
# press$donorname[which(!(press$donorname %in% reporters$donorname_unified))] %>% unique 
# press$donorname[which(!(press$donorname %in% reporters$ReporterName))] %>% unique


# df_crs_reporters <- df_crs %>% select(donorname, donorcode) %>% unique %>% 
#   arrange(donorname) 
# 
# df_reporters1 %>%  select(donorname = crs_name_en, donorcode = crs_code) %>% unique %>% arrange(donorname) %>% 
#   inner_join(df_crs_reporters)
# df_reporters1 is completely compatible with df_crs donors 