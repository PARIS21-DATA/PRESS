
# fig 2.1: income classes
x <- df_crs
# 1. share of funding in each year_recipient groups
fun_summarise_ts(x, 
                 group_var = "incomegroupname",
                 rolling = T, 
                 roll_period = 2) %>% 
  gather(key = "incomegroupname", 
         value = "total", 
         -group_year) %>% 
  group_by(group_year) %>% 
  mutate(sum = sum(total, na.rm = T)) %>% 
  mutate(share = total/sum)

# 2 growth rate
## 2.1 annual growth rate
e_fig$byDonorType_byRecipientType_growth <- x %>% 
  mutate(group_year = year) %>% 
  group_by(incomegroupname, donor_type, group_year) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  arrange(incomegroupname, donor_type, group_year) %>% 
  group_by(incomegroupname, donor_type)%>%
  mutate(baseline = first(total)) %>% 
  mutate(growth_rate = (total/baseline - 1) * 100) %>%
  ungroup() %>% 
  filter(!is.na(growth_rate))  

e_fig$byDonorType_byRecipientType_growth
# e_fig$byDonorType_byRecipientType_growth %>% 
#   write.xlsx("output/press/charts PRESS 2023/fig 2.1 bi_multi by recipients.xlsx") 

## 2.2 rolling growth rate
roll_period = 2
start_year = 2010
e_fig$roll_byDonorType_byRecipientType_growth <- x %>% 
  mutate(group_year = year
         ,
         group_year =  (group_year - start_year) %/% roll_period
         , group_year = group_year* roll_period,
         group_year_start = group_year + start_year,
         group_year_end = group_year + start_year + roll_period - 1, 
         group_year = paste0(group_year_start, "-", group_year_end)
  ) %>% 
  group_by(group_year, donor_type, incomegroupname) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  arrange(incomegroupname, donor_type, group_year) %>% 
  group_by(incomegroupname, donor_type)%>%
  mutate(baseline = first(total)) %>%
  mutate(growth_rate = (total/baseline - 1) * 100) %>%
  # mutate(growth_rate = total) %>% 
  ungroup() %>% 
  filter(!is.na(growth_rate))  

### 2.2.2 final grid chart

# e_fig$byDonorType_byRecipientType_growth %>% 
e_fig$roll_byDonorType_byRecipientType_growth %>%
  # rename(year = group_year) %>% 
  filter(incomegroupname %in% c("LDCs", "LMICs",
                                "UMICs",
                                "Part I unallocated by income")) %>%
  mutate(incomegroupname = ifelse(incomegroupname == "Part I unallocated by income", 
                                  "Regional projects", 
                                  incomegroupname)) %>% 
  mutate(group_year = ifelse(group_year == "2010-2012", 
                             "2010-2012\n(baseline)", 
                             group_year)) %>% 
  filter(donor_type != "Private") %>% 
  ggplot( aes(x = group_year, y = growth_rate, group = donor_type)) +
  geom_line(aes(linetype = donor_type, color = donor_type)) +
  scale_linetype_manual(values = c("solid",
                                   # "dotted",
                                   "dashed")) +
  labs(# title = "Evolution of Funding by Recipient Type",
    y = "Growth Rate (%)",
    x = "Year", 
    color = "Donor Type"   # Change title for color legend
    ,linetype = "Donor Type") + # Change title for linetype legend) +
  facet_wrap(~incomegroupname, ncol = 2, scales = "free_y") +  # Adjust ncol as needed
  theme_minimal()

# 3 MINOR double check if the there are also regional ones in each individual groups

x %>% 
  filter(incomegroupname !=  "Part I unallocated by income") %>% 
  filter(grepl("region|région|unspecif", recipientname, ignore.case = T) ) %>% 
  select(recipientname) %>% 
  distinct

x %>% 
  filter(incomegroupname ==  "Part I unallocated by income") %>% 
  # filter(grepl("region|région", recipientname, ignore.case = T) ) %>% 
  select(recipientname) %>% 
  distinct %>% 
  slice(11:22)

x %>% 
  filter(incomegroupname == "Part I unallocated by income") %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  # select(ch_name, recipientname, usd_disbursement_defl)
  select(recipientname) %>% 
  distinct %>% 
  slice(1:20)


x %>% 
  filter(donor_type == "Multilateral") %>% 
  fun_summarise_ts(group_var = "incomegroupname",
                   rolling = T, 
                   roll_period = 3)


x %>% 
  filter(donor_type == "Bilateral") %>% 
  fun_summarise_ts(group_var = "incomegroupname",
                   rolling = T, 
                   roll_period = 3)

x %>% 
  filter(donor_type == "Bilateral") %>% 
  filter(year > 2018) %>% 
  filter(incomegroupname == "Part I unallocated by income") %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, 
         channelname,
         channelreportedname, 
         # recipientname, 
         projecttitle, 
         usd_disbursement_defl) %>% 
  select(channelname) %>% 
  distinct

# 4. left overs
### 4.1 2.1.1 chart 
### the chart is to chaotic with annual rate
e_fig$byDonorType_byRecipientType_growth %>% 
  filter(incomegroupname %in% c("LDCs", "LMICs",
                                "UMICs",
                                "Part I unallocated by income")) %>%
  filter(donor_type != "Private") %>% 
  filter(donor_type != "Multilateral") %>% 
  ggplot( aes(x = group_year, y = growth_rate, group = interaction(donor_type, incomegroupname))) +
  geom_line(aes(linetype = donor_type, color = incomegroupname)) +
  scale_linetype_manual(values = c("solid", 
                                   # "dotted", 
                                   "dashed")) +
  labs(title = "Growth Rate by Donor Type and Income Group",
       y = "Growth Rate (%)",
       x = "Year") +
  theme_minimal()

### 4.2 - 2.2.1 in one chart
e_fig$roll_byDonorType_byRecipientType_growth %>% 
  # rename(year = group_year) %>% 
  filter(incomegroupname %in% c("LDCs", "LMICs",
                                "UMICs",
                                "Part I unallocated by income")) %>%
  filter(donor_type != "Private") %>% 
  # filter(donor_type != "Multilateral") %>% 
  ggplot( aes(x = group_year, y = growth_rate, group = interaction(donor_type, incomegroupname))) +
  geom_line(aes(linetype = donor_type, color = incomegroupname)) +
  scale_linetype_manual(values = c("solid", 
                                   # "dotted", 
                                   "dashed")) +
  labs(title = "Growth Rate by Donor Type and Income Group",
       y = "Growth Rate (%)",
       x = "Year") +
  theme_minimal()
