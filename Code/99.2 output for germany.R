df_crs_og_og <-  readRDS("Data/Raw/CRS/crs_de.rds")
positive_text_id <- read_rds(paste0("./Data/intermediate/positive_text_id", "_de", ".rds"))
df_crs_og_og$agencyname %>% unique

# source("Code/04. create matrix.R")

rm(list = ls()[!ls() %in% c("positive_text_id", "df_crs_og_og")])

df_crs_after3 <- readRDS("Data/Intermediate/crs03_de.rds")
a = df_crs_after3 %>% 
  filter(text_detection_wo_mining_w_scb) %>%
  .$process_id%>%
  unique

c = df_crs_after3 %>% 
  filter(scb==1) %>%
  .$process_id%>%
  unique

b = df_crs_after3 %>%
  filter(text_id %in% positive_text_id) %>%
  .$process_id



positive_ones = c(a,b) %>% unique 

from_code = c

from_text = positive_ones[!positive_ones %in% from_code] 
# rm(a,b)

df_crs_og <-  readRDS("Data/Intermediate/crs01_1_de.rds")

df_crs_og_positive = df_crs_og %>% 
  filter(process_id %in% positive_ones) %>%
  mutate(source = ifelse(process_id %in% from_code, "Purpose code: statistical capacity building", "Text mining"))
  

names(df_crs_og_positive)

write.csv(df_crs_og_positive, file = "Data/Germany Projects.csv", row.names = F)
saveRDS(df_crs_og_positive, file = "Data/Germany Projects.rds")


original_d4d <- read_csv("Output/Germany/dcd.csv")


ge_d4d = original_d4d %>%
  filter(DonorName == "Germany")

output = df_crs_og_positive

not_captured = ge_d4d %>%
  filter(!CrsID %in% output$crsid)
not_captured$source %>% table
not_captured %>%
  filter(source == "Text mining") %>% 
  .$ProjectTitle %>% unique


not_captured %>%
  filter(source != "Text mining") %>% 
  .$PurposeCode

a = not_captured %>%
  filter(source == "Text mining") %>% 
  .$CrsID 
b = df_crs_og %>% 
  filter(crsid %in% a)

b$projecttitle %>% unique

c = b$projecttitle %>% unique
c = c[c(1:4 , 7 , 13:17, 27,28, 31, 34, 36)]

# c = c[c(2,3,7,8,10,11,14,20, 21, 24, 27,29)]
b1 = b %>% filter(projecttitle %in% c )
b1$projecttitle %>% unique


df_crs_og_positive = df_crs_og_positive %>% rbind(b1)



df_crs_og_positive = df_crs_og_positive %>%
  mutate(source = ifelse(process_id %in% from_code, "Purpose code: statistical capacity building", "Text mining"))

which(duplicated(df_crs_og_positive$process_id))


d4d_names <- data.frame(original = names(original_d4d), stringsAsFactors = F) %>% 
  mutate(lower_original = tolower(original))  

which(!(names(df_crs_og) %in% d4d_names$lower_original))
names(df_crs_og)[which(!(names(df_crs_og) %in% d4d_names$lower_original))] 

which(!( d4d_names$lower_original %in% names(df_crs_og) ))

d4d_names$original[which(!( d4d_names$lower_original %in% names(df_crs_og) ))]

original_d4d$ParentChannelCode %>% table

d4d_names <- d4d_names %>% 
  filter(lower_original %in% names(df_crs_og_positive)) 

df_crs_og_positive_reduced <- df_crs_og_positive %>%
  select(all_of(d4d_names$lower_original))

names(df_crs_og_positive_reduced)
d4d_names$original
(original_d4d$source) %>% table

names(df_crs_og_positive_reduced) == d4d_names$lower_original

names(df_crs_og_positive_reduced) = d4d_names$original

write.csv(df_crs_og_positive_reduced, file = "Data/Germany Projects.csv", row.names = F)
saveRDS(df_crs_og_positive_reduced, file = "Data/Germany Projects.rds")

df_crs_og_positive_reduced %>% .$AgencyName %>% unique

# df_crs_og_og %>% filter(donorname == "Germany") %>% .$agencyname 
  # table()
