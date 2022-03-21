

load("Analysis/reporters_2021.rdata")

write.csv(reporters, file = "./analysis/reporters2021.csv", row.names = F)

load("Analysis/iati_donors_2021.rds")
write.csv(donors_fixed, file = "./analysis/iati_reporters_2021.csv", row.names = F)


