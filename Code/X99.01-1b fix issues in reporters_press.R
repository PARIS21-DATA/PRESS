# Arab fund for ec
df_reporters_press$donorcode[193] <-  921
df_reporters_press$ReporterId[193] <- 80
# hewlett
df_reporters_press$donorcode[c(144, 184)] <- 1624
df_reporters_press$ReporterId[c(144, 184)] <- 1624
# unece
df_reporters_press$donorcode[173] <-  948
df_reporters_press$ReporterId[173] <- 948

df_reporters_press[c(193,144, 184, 173 ),]
write_rds(df_reporters_press, file = "Data/auxiliary/reporters.rds")



df_reporters_press %>% write.csv("Data/analysis/reporters_survey.csv")

df_reporters2021 <- read_csv("data/Analysis/ReporterList202112.csv")

a <- df_survey_reporters$donorcode_unified %in% df_reporters2021$ReporterId
b <- df_survey_reporters$ReporterId %in% df_reporters2021$ReporterId
unmatched <- which(!(a|b) )
df_survey_reporters$donorname_survey[unmatched] %in% df_survey_reporters_unmateched$donorname_survey
df_survey_reporters_unmateched$donorname_survey %in%  df_survey_reporters$donorname_survey[unmatched] 
df_survey_reporters_unmateched2 <- df_survey_reporters[unmatched,]