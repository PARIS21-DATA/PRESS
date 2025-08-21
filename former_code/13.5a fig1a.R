
avg_disb_2019_2021 <- mean(e_output$fig_1$Disbursements[e_output$fig_1$year %in% 2019:2021])

p1 <- ggplot(e_output$fig_1, aes(x = year)) +
  geom_bar(aes(y = Commitments, fill = "Commitments"), stat = "identity") +
  geom_line(aes(y = Disbursements, color = "Disbursements", linetype = "Disbursements"), size = 2) +
  geom_line(data = subset(e_output$fig_1, year %in% 2019:2021),
            aes(x = year, y = rep(avg_disb_2019_2021, sum(year %in% 2019:2021)),
                color = "Avg. Disbursements 2019-2021", 
                linetype = "Avg. Disbursements 2019-2021"),
            size = 2) +
  labs(
    title = "Commitments and Disbursements",
    subtitle = "2021 constant USD, millions",
    x = NULL, y = NULL,
    caption = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = e_output$fig_1$year) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Disbursements" = "#04456A", "Avg. Disbursements 2019-2021" = "#BA704F"),
                     name = "") +
  scale_fill_manual(values = c("Commitments" = "#BCBFA4"), name = "") +
  scale_linetype_manual(values = c("Disbursements" = "solid", "Avg. Disbursements 2019-2021" = "solid"),
                        name = "")


p1 <- p1 +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) 

# Display the plot
# print(p1)
# output_path_fig <- paste0("output/CH/D4D Validation/Charts/", 
#                           var_donor_working, 
#                           "_fig1a.svg")
# ggsave(output_path_fig,p1)
