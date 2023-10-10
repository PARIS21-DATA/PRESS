library(dplyr)
library(ggplot2)
library(scales)

# Sample data
# Note: Always ensure to check with your actual data
df <- e_output$fig_5 %>% ungroup 

# Step 1: Calculate average for 2019-2021 and append
avg_data <- df %>%
  filter(year %in% 2019:2021) %>%
  select(year, aid_t_name, share) %>% 
  spread(key = aid_t_name, value = share, fill = 0) %>%
  gather(key = "aid_t_name", value = "share", 
         -year) %>%
  mutate(share = share/3) %>% 
  group_by(aid_t_name) %>% 
  summarise(share = sum(share)) %>% 
  ungroup() %>%
  mutate(year = 2022) %>% 
  left_join(df %>% select(aid_t_name, aid_t) %>% distinct)

# Add a gap bar (all zero) for 2022 for visual space before the average bar
gap_data <- avg_data %>% 
  distinct(aid_t_name, aid_t) %>%
  mutate(share = 0, year = 2021.5)  

# Append the original and average data
final_data <- rbind(df, gap_data, avg_data)

rm(df, avg_data, gap_data)


# Step 2: Define color mapping based on aid_t
# Formal report palette (Ensure colors are distinct and choose more if needed)
# formal_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
#                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

df_aid_t <- read_feather("data/intermediate/13.5 aid type names.feather")

df_aid_t$aid_t_name


# df_aid_t <- df_aid_t %>% 
#   mutate(aid_t_name_new = str_wrap(aid_t_name, 30))

# df_aid_t$aid_t_name_new %>% print

final_data <- df_aid_t %>%
  inner_join(final_data)

final_data_colour <- final_data %>% 
  select(aid_t_name, palette) %>% 
  distinct

# Assigning colors based on unique `aid_t`
col_assign <- setNames(final_data_colour$palette, 
                       final_data_colour$aid_t_name)

rm(final_data_colour)

# Creating a new column 'color' assigning colors based on 'aid_t'
final_data <- final_data %>% 
  mutate(color = col_assign[aid_t_name]) 

final_data <- final_data %>% 
  ungroup %>% 
  arrange(year, aid_t)

# Assuming col_assign contains the labels for the legend
wrapped_labels <- str_wrap(names(col_assign), width = 25)  # Adjust width as needed

final_data$color <- factor(final_data$color, levels = df_aid_t$palette)

# Step 3: Building the plot
p <- ggplot(final_data, aes(x = as.factor(year), y = share, fill = color)) +
  geom_bar(stat = "identity", 
           # show.legend = TRUE, 
           position = "stack") +
  geom_text(aes(label = ifelse(share >= 1, paste0(sprintf("%.0f", round(share,0)), "%"), NA)),
            position = position_stack(vjust = 0.5),
            # size = 3, # Adjust text size
            color = "black" # Adjust text color
  )  +
  scale_x_discrete(limits = c(as.character(2010:2021), "2022"), 
                   breaks = c(as.character(2010:2021), "2022"),
                   labels = c(as.character(2010:2021), "2019-21\nAverage")) +
  scale_fill_identity(guide = "legend", 
                      breaks = col_assign,
                      labels = wrapped_labels) +  # Use wrapped labels
  labs(
    title = "ODA to data and statistics by type of aid",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(reverse = TRUE, 
                             keywidth = unit(1, "cm"),  # Specify key width
                             keyheight = unit(0.5, "cm"), # Specify key height
                             label.position = "bottom"))  # Adjust label position accordingly

p <- p + 
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))


# Display the plot
print(p)


rm(col_assign, wrapped_labels)

rm(final_data)

output_path_fig <- paste0("output/CH/D4D Validation/Charts/", 
                          var_donor_working, 
                          "_fig3.png")
ggsave(output_path_fig,p)

