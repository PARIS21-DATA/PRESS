
# 0. prepare the environment 
# source("code/00. boot.R")
path_input_excel <- "documents/D4D dataset template 2023.xlsx"
## 0.1 create a new env 
e_template <- new.env()
vec_d4d_donors <- readLines("data/D4D/d4d donors.txt")
vec_d4d_donors %in% unique(df_crs$dac_donorname)



# 1. prepare the master dataset

## 1.0 understand the master sheet
e_template$master <- read.xlsx("Documents/D4D dataset template.xlsx", 
                               sheet = "master")
names(e_template$master)

## 1.1 create the lower cases of names to be consistent with df_crs
vec_master_names_lower <- tolower(names(e_template$master))

setdiff(vec_master_names_lower, 
        names(df_crs))

## 1.2 create a tibble to convert names
e_template$names_master <- tibble(names_original = names(e_template$master), 
                                  names_new = vec_master_names_lower) %>% 
  mutate(order = 1:n())

## 1.3 "sector" is not available in the original dataset

e_template$master %>% 
  select(sector, PurposeCode) %>% 
  distinct %>% 
  arrange(PurposeCode, sector) 

df_crs_d4d <- df_crs %>% 
  mutate(donorname = dac_donorname, 
         crsid = db_original_id, 
         sector = sectorname, 
         type_channel = channelcode) %>% 
  select(all_of(e_template$names_master$names_new))
names(df_crs_d4d) <- e_template$names_master$names_original

df_crs_d4d %>% write_feather("data/intermediate/13.2 master data for D4D.feather")

## 1.4 understand other sheets
e_template$review <- read.xlsx("Documents/D4D dataset template.xlsx", 
                               sheet = "Review -  2020 D4D Projects ")
names(e_template$review)

rm(e_template)

# 2. output data

e_output <- new.env()
wb <- loadWorkbook(path_input_excel)
removeWorksheet(wb, "Lapsed")
removeWorksheet(wb, "Review -  2020 D4D Projects ")
removeWorksheet(wb, "by year and agency")
removeWorksheet(wb, "by channel and agency")
removeWorksheet(wb, "master")
removeWorksheet(wb, "by channel and agency 2018-20")
removeWorksheet(wb, "by purpose code and agency")

## 2.1 load donor to work with and clean the workbook


for (i in vec_d4d_donors) {
  var_donor_working <- i
  source("code/13.2 D4D loop to save data to excel sheet.R")
}
