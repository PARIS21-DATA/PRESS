p <- ggplot(e_output$fig_2, aes(x = year)) +
  geom_line(aes(y = net, color = "% of net ODA"), size = 1) +
  geom_line(aes(y = bilateral, color = "% of bilateral ODA"), size = 1) +
  geom_line(aes(y = CPA, color = "% of CPA"), size = 1) +
  labs(
    title = "Commitments and disbursements",
    subtitle = "2021 constant USD, millions",
    x = NULL, y = NULL,
    caption = NULL,
    colour = NULL  # Add this line to remove the legend title
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = e_output$fig_2$year) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, max(c(e_output$fig_2$net, e_output$fig_2$bilateral, e_output$fig_2$CPA), na.rm = TRUE)))

# Display the plot
print(p)
output_path_fig <- paste0("output/CH/D4D Validation/Charts/", 
                          var_donor_working, 
                          "_fig1b.png")
ggsave(output_path_fig,p)
