setwd("~/Dropbox/PARIS21/PRESS/2021/")


load("iati_2021_marked_w_iso_region.rds")
head(iati$hierarchy)

table(iati$currency)
table(iati$default.currency)

# iati$stats = 
