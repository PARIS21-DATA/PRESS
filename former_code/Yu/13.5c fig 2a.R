
# 1. Data Preprocessing

# Convert to title-case
e_output$fig_3 <- e_output$fig_3 %>% 
  mutate(marker = case_when(marker == "rmnch" ~ "reproductive,\nmaternal,\nnewborn\nand child\nhealth", 
                            marker == "pdgg" ~ "participatory\ndevelopment,\ngood\ngovernance", 
                            TRUE ~ marker))
e_output$fig_3$marker <- tools::toTitleCase(e_output$fig_3$marker)


# Convert to long format
e_output$fig_3_long <- e_output$fig_3 %>%
  pivot_longer(cols = -marker, names_to = "category", values_to = "share") %>%
  mutate(
    category = factor(category, levels = c("<NA>", "0", "1", "2"),
                      labels = c("Not screened", "Not an objective", "Partial objective", "Principal objective")),
    direction = ifelse(category %in% c("Not an objective", "Not screened"), "left", "right")
  ) %>%
  # mutate(category = as.factor(category)) %>% 
  group_by(marker, direction) %>%
  arrange(desc(category)) %>%
  mutate(cumsum = cumsum(share)) %>%
  ungroup()
# Add total column to represent the sum of `1` and `2` for each marker
e_output$fig_3_long <- e_output$fig_3_long %>%
  group_by(marker) %>%
  mutate(total = sum(share[category %in% c("Partial objective", "Principal objective")])) %>%
  ungroup() %>% 
  arrange(marker, category)

# Define colors
color_map <- c("Not screened" = "grey", 
               "Not an objective" = "#708090",
               "Partial objective" = "#5B8CA2", 
               "Principal objective" = "#04456A")



# Assume e_output$fig_3_long is available as specified

# Computing label positions
label_data <- e_output$fig_3_long %>% 
  filter(category %in% c("Partial objective",
                         "Principal objective")) %>% 
  group_by(marker) %>% 
  summarise(label_var = sum(share),
            label_side = ifelse(first(direction) == "left", -1, 1)) %>% 
  ungroup() %>% 
  mutate(label_pos = label_var, 
         category = "")

# 2. Building the Plot
p1 <- ggplot(e_output$fig_3_long, aes(x = marker, y = share, fill = category)) +
  geom_bar(data = ~ subset(., direction == "left"), 
           aes(y = -share), stat = "identity") +
  geom_bar(data = ~ subset(., direction == "right"),
           stat = "identity") +
  geom_text(data = label_data, 
            aes(x = marker, y = label_side*label_pos, label = sprintf("%.1f%%", label_var)),
            vjust = -0.5, # adjust to position above the bar
            hjust = 0.5) + # centered on the bar
  
  # 3. Customizing Appearances
  
  scale_fill_manual(values = color_map) +
  labs(
    title = "By policy objective",
    x = NULL, y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) 

# rm(label_data)

p1 <- p1 +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    # axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  ) +
  labs(
    title = "By Policy Objectives",
    # subtitle = "2021 constant USD, millions",
    x = "",
    y = ""
  )

# Display the plot
print(p1)

# output_path_fig <- paste0("output/CH/D4D Validation/Charts/", 
#                           var_donor_working, 
#                           "_fig2a.png")
# ggsave(output_path_fig,p1)



# baseline chart

# p <- ggplot(e_output$fig_3_long, aes(x = marker, y = share, fill = category)) +
#   geom_bar(data = ~ subset(., direction == "left"), 
#            aes(y = -share), stat = "identity") +
#   geom_bar(data = ~ subset(., direction == "right"),
#            stat = "identity") +
#   # coord_flip() +
#   
#   # 3. Customizing Appearances
#   
#   scale_fill_manual(values = color_map) +
#   labs(
#     title = "By policy objective",
#     x = NULL, y = NULL,
#     fill = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "top",
#         axis.text.y = element_blank()) +
#   guides(fill = guide_legend(reverse = TRUE))
# 
# # Display the plot
# print(p)
