# Define base colors for each group to ensure they are distinct from each other
base_colors <- c(

  "groupA" = "#2ca02c", # green
  "groupB" = "#1f77b4", # blue

  "groupC" = "#9467bd", # purple

  "groupD" = "#e377c2",  # pink
  "groupE" = "#d62728", # red
  "groupH" = "#ff7f0e", # orange
  "groupG" = "#8c564b" # brown

)

# Function to create a set of n lighter/darker shades of a color
create_shades <- function(color, n) {
  colorRampPalette(c("white", color))(n + 2)[2:(n+1)]
}

# Define shades for each group
palette <- c(
  create_shades(base_colors["groupA"], 2), # 2 shades of blue
  create_shades(base_colors["groupB"], 3), # 3 shades of orange
  base_colors["groupC"], # 1 shade of green
  create_shades(base_colors["groupD"], 2), # 2 shades of red
  create_shades(base_colors["groupE"], 2), # 2 shades of purple
  base_colors["groupG"], # 1 shade of brown
  create_shades(base_colors["groupH"], 2)  # 2 shades of pink
)

palette


df_aid_t <- df_crs_d4d %>% select(aid_t, aid_t_name) %>% distinct %>% 
  mutate(aid_t_full = aid_t) %>% 
  mutate(aid_t = substr(aid_t,1,1)) %>% distinct %>%
  filter(!is.na(aid_t_name)) %>% 
  arrange(aid_t, aid_t_name) %>% 
  mutate(palette = palette)

write_feather(df_aid_t, "data/intermediate/13.5 aid type names.feather")
