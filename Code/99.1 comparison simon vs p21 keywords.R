p21 = readRDS("Data/list_by_P21.rds") %>% mutate(P21 = "x")
simon = read_rds("Data/list_by_simon.rds" ) %>% mutate(d4d = "x")

comparison = full_join(p21, simon) %>% filter(!is.na(text_id))
write_csv(comparison, file = "data/comparison.csv")
