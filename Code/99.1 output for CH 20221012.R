rm(list = ls())

load("data/Intermediate/06.1 crs and press with region and country code.rdata")

df_press <- read.csv("Data/Analysis/PRESS 202112.csv", 
         encoding = "utf-8")


tolower(names(df_press) )%in% names(df_crs)

df_crs$cur

df_crs %>% 
  select(db_ref, 
         db_original = db_original_id, 
         ProgramName = projecttitle,
         Objectives = longdescription, 
         ExpectedEndDate = completiondate, 
         ListRecip = recipientname, 
         commitmentCurrency = NA, 
         Commitment = commitment_national, 
         usd_disbursements = usd_disbursement, 
         CommitmentDate = commitmentdate, 
         endyear = completiondate, 
         RecipientCode = dac_recipientcode, 
         usd_commitment = usd_commitment, 
         ReporterName = donorname, 
         ReporterId = donorcode, 
         ReporterType = NA ,
         source  = "CRS", 
         bi_multi
         )



df_press %>% filter(ReporterName == "France") %>% select(ProgramName) %>% 
  head %>% 
  write.csv("data/Analysis/text_france_output.csv", 
            fileEncoding = "macintosh")
