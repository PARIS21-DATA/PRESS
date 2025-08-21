# Check whether project identifier is unique
print(paste0("Number of unique project identifiers: ", df_crs$db_ref %>% unique %>% length))
print(paste0("Number of rows of crs data set: ", nrow(df_crs)))
print(paste0("Number of duplicated project identifiers: ", sum(duplicated(df_crs$db_ref))))
print("Identifiers that are duplicated:")
df_crs$db_ref[which(duplicated(df_crs$db_ref))] %>% print 
print("Subset of crs with identical project identifiers: ")
dups = c(which(duplicated(df_crs$db_ref)) , which(rev(duplicated(rev(df_crs$db_ref)))))
df_crs[ dups, ] %>% arrange(db_ref)%>% print
