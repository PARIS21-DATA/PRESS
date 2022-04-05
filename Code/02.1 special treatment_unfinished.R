
crs = crs %>% 
  filter(((longdescription != "TC AGGREGATED ACTIVITIES" )& (projecttitle!= "TC AGGREGATED ACTIVITIES")) | (SCB == 1)) %>%
  select(projectID, SCB, POP, gen_ppcode, description_comb,projecttitle)