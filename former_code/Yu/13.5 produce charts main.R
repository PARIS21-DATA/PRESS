rm(list = ls())
source("code/00. boot.R")
time_start <- Sys.time()
df_crs_d4d <- read_feather("data/Intermediate/13.3 d4d data ready for charts 2023.feather")
vec_d4d_donors <- readLines("Data/D4D/d4d donors.txt")
## 0.2 prepare aid type names

df_aid_t <- read.xlsx("data/auxiliary/Aid type names 2023.xlsx")

df_crs_d4d <- df_crs_d4d %>% 
  left_join(df_aid_t)

rm(df_aid_t)

# i = "Canada"

for (i in vec_d4d_donors) {
  var_donor_working <- i
  df_crs_d4d_donor <- df_crs_d4d %>% 
    filter(dac_donorname == var_donor_working)
  
  path_input_rdata <- paste0("data/intermediate/13.4 ", 
                             var_donor_working, 
                             " figure data.Rdata")
  load(path_input_rdata)
  # library(gridExtra)
  source("code/13.5a fig1a.R")
  source("code/13.5b fig1b.R")
  p <- grid.arrange(p1, p2, nrow = 1)
  # print(p)
  output_path_fig <- paste0("output/CH/D4D Validation/Charts/",
                            var_donor_working,
                            "_fig1.svg")
  ggsave(output_path_fig,p,width = 9.97/2*2.6, height = 4.89/2*2.6)
  
  # output_path_fig_rds <- paste0("output/CH/D4D Validation/Charts/",
  #                           var_donor_working,
  #                           "_fig1.rds")
  # saveRDS(p, output_path_fig_rds)
  source("code/13.5c fig 2a.R")
  source("code/13.5d fig 2b.R")
  # p <- grid.arrange(p1, p2, nrow = 1)
  p <- grid.arrange(p1, p2, nrow = 1, widths = c(1.5, 1))
  
  # print(p)
  output_path_fig <- paste0("output/CH/D4D Validation/Charts/",
                            var_donor_working,
                            "_fig2.svg")
  ggsave(output_path_fig,p,width = 9.97/2*2.6, height = 4.89/2*2.6)
  
  # output_path_fig_rds <- paste0("output/CH/D4D Validation/Charts/",
  # var_donor_working,
  # "_fig2.rds")
  # saveRDS(p, output_path_fig_rds)
  source("code/13.5e fig3.R")

  # output_path_fig_rds <- paste0("output/CH/D4D Validation/Charts/",
                                # var_donor_working,
                                # "_fig3.rds")
  # saveRDS(p, output_path_fig_rds)
  
  print(i)
}

print_time_diff(time_start)



