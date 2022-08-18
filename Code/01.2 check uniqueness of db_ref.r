df_crs$db_ref %>% unique %>% length %>% print
nrow(df_crs)%>% print
which(duplicated(df_crs$db_ref))%>% print
df_crs$db_ref[which(duplicated(df_crs$db_ref))]%>% print 
dups = c(which(duplicated(df_crs$db_ref)) , which(rev(duplicated(rev(df_crs$db_ref)))))%>% print
df_crs[ dups, ] %>% arrange(db_ref)%>% print
