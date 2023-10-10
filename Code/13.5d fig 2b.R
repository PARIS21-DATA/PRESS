

names(e_output$fig_4)[2] <- var_donor_working
names(e_output$fig_4)[3] <- "Total DAC"

# Ensure 'Other' is the last factor level
e_output$fig_4$sector_label <- factor(e_output$fig_4$sector_label, 
                                      levels = c("General statistical capacity", "Health", "Gender", "Population", "Education", "Economy", "Agriculture", "Environment", "Other"))
# Convert data to long format
df_long <- e_output$fig_4 %>%
  pivot_longer(cols = c(var_donor_working, "Total DAC"), names_to = "country", values_to = "value") 

# Distinguishable, print-friendly and colorblind-friendly colors
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Assign colors to each level (using additional grey for "Other")
col_assign <- setNames(c( okabe_ito_palette, "white"), levels(e_output$fig_4$sector_label))

p <- ggplot(df_long, aes(x = country, y = value, fill = sector_label)) +
  geom_bar(stat = "identity") +
  geom_bar(data = subset(df_long, sector_label == "Other"), 
           aes(y = value), fill = NA, color = "black", linetype = "dotted", size = 0.5,
           stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = col_assign) +
  labs(
    title = "By statistical domain",
    x = NULL, y = NULL,
    fill = NULL  # This line removes the legend title
  ) +
  geom_text(aes(label = ifelse(value >= 3, percent(value/100, accuracy = 1), "")), 
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  # theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Print the plot
print(p)

