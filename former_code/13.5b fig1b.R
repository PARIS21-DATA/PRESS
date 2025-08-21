
p2 <- ggplot(e_output$fig_2, aes(x = year)) +
  geom_line(aes(y = net, color = "% of net ODA"), size = 2) +
  geom_line(aes(y = bilateral, color = "% of bilateral ODA"), size = 2) +
  geom_line(aes(y = CPA, color = "% of CPA"), size = 2) +
  labs(
    title = "Disbursements",
    subtitle = "% of total ODA",
    x = NULL, y = NULL,
    caption = NULL,
    colour = NULL  # Add this line to remove the legend title
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = e_output$fig_2$year) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, max(c(e_output$fig_2$net, e_output$fig_2$bilateral, e_output$fig_2$CPA), na.rm = TRUE)))

# Adjusting p2
p2 <- p2 +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) 

# rgb_values
# a <- rgb(189, 140, 214, maxColorValue = 255)
# b <- rgb(109, 140, 162, maxColorValue = 255)
# c <- rgb(20, 140, 86, maxColorValue = 255)
# rgb(154, 170, 163, maxColorValue = 255)
p2 <- p2 + 
  scale_color_manual(values = c(`% of net ODA` = "#BA704F", 
                                `% of bilateral ODA` = "#9AAAA3", 
                                `% of CPA` = "#04456A"))

# Display the plot
print(p2)
# output_path_fig <- paste0("output/CH/D4D Validation/Charts/", 
#                           var_donor_working, 
#                           "_fig1b.png")
# ggsave(output_path_fig,p2)
