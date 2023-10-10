
# 0. load ODA data
df_net_oda <- read.xlsx("data/d4d/Total ODA.xlsx")
df_bilat_oda <- read.xlsx("data/d4d/Bilateral ODA.xlsx")
df_cpa_oda <- read.xlsx("data/d4d/CPA ODA.xlsx")

fun_lower_names <- function(x) {
  names(x) <- tolower(names(x))
  return(x)
}

df_net_oda <- fun_lower_names(df_net_oda)
df_bilat_oda <- fun_lower_names(df_bilat_oda)
df_cpa_oda <- fun_lower_names(df_cpa_oda)

df_net_oda <- df_net_oda %>% 
  select(dac_donorcode = dac_donor
         , donorname = donor
         , year = time
         , usd_disbursement_defl = value) %>% 
  mutate(oda_type = "net")


df_bilat_oda <- df_bilat_oda %>% 
  select(dac_donorcode = dac_donor
         , donorname = donor
         , year = time
         , usd_disbursement_defl = value) %>% 
  mutate(oda_type = "bilateral")
names(df_cpa_oda)[2] <- "donorname"
df_cpa_oda <- df_cpa_oda %>% 
  select(dac_donorcode = donor
         , donorname
         , year 
         , usd_disbursement_defl = value) %>% 
  mutate(oda_type = "CPA")


df_oda <- rbind(df_net_oda, 
                df_bilat_oda, 
                df_cpa_oda)

rm(df_net_oda, df_bilat_oda, df_cpa_oda)

vec_d4d_donors %in% unique(df_oda$donorname)

write_feather(df_oda , "data/d4d/ODA for D4D calculation.feather")

## 0.1 prepare crs aggregated data

e_output$sector_shares <- df_crs_d4d %>% 
  group_by(sector_label) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  ungroup %>% 
  mutate(share_total_oda = round(total/sum(total)*100, 1)) %>% 
  select(-total)
e_output$sector_shares

## 0.2 prepare aid type names

df_aid_t <- read.xlsx("data/auxiliary/Aid type names 2023.xlsx")

df_crs_d4d <- df_crs_d4d %>% 
  left_join(df_aid_t)

rm(df_aid_t)

# 1. load d4d data for a specific country

var_donor_working <- vec_d4d_donors[1]
df_crs_d4d_donor <- df_crs_d4d %>% 
  filter(dac_donorname == var_donor_working)

names(df_crs_d4d)
# figure 1
e_output$fig_1 <- df_crs_d4d_donor %>% 
  group_by(year) %>% 
  summarise(Commitments = sum(usd_commitment_defl, na.rm = T), 
            Disbursements = sum(usd_disbursement_defl, na.rm = T)) %>% 
  ungroup %>% 
  group_by(year > 2018) %>% 
  mutate(`Average disburssements, 2019-21` = ifelse(year <= 2018, 
                                                    NA, 
                                                    mean(Disbursements)))  %>% 
  ungroup %>% 
  select(-`year > 2018`)
e_output$fig_1 

# fig 2

e_output$fig_2 <- df_oda %>% 
  select(-dac_donorcode) %>% 
  filter(donorname == var_donor_working) %>% 
  select(-donorname) %>% 
  spread(key = oda_type, value = usd_disbursement_defl) %>% 
  inner_join(e_output$fig_1 %>% select(year, Disbursements)) %>% 
  mutate(across(bilateral:net, ~ round(Disbursements/.x*100,1))) %>% 
  select(-Disbursements)
e_output$fig_2


# fig 3
e_output$fig_3 <- df_crs_d4d_donor %>% 
  filter(year >2018) %>% 
  select(usd_disbursement_defl, 
         trade, 
         disability, 
         nutrition, 
         rmnch, 
         environment, 
         gender, pdgg) %>% 
  gather(key = "marker", 
         value = "value", -usd_disbursement_defl) %>% 
  group_by(marker, value) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  group_by(marker) %>% 
  mutate(share = round(total/sum(total)*100, 1)) %>% 
  select(-total) %>% 
  spread(key = value, value = share, fill = 0) 
e_output$fig_3
# fig 4
e_output$fig_4 <- df_crs_d4d_donor %>% 
  filter(year >2018) %>% 
  group_by(sector_label) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  ungroup %>% 
  mutate(share = round(total/sum(total)*100, 1)) %>% 
  select(-total) %>% 
  right_join(e_output$sector_shares)



# fig 5

e_output$fig_5 <- df_crs_d4d_donor %>% 
  # filter(year >2018) %>% 
  group_by(year, aid_t_name) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(share = round(total/sum(total)*100, 1)) %>% 
  select(-total) %>%
  spread(key = aid_t_name, 
         value = share, 
         fill = 0)
  # right_join(e_output$sector_shares)
e_output$fig_5 
