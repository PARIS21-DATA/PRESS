

df <- df_crs_recipients %>% ungroup %>% 
  select(-year_sum, -top5_share)

df_bar <- df %>% 
  select(-top25_share)  %>% 
  # mutate(rank = as.character(rank))
  mutate(rank = rank_text)

df_line <- df %>% 
  select(year, share = top25_share) %>% 
  mutate(isocode = "Top 25"
         , rank = "5th"
         ) %>% 
  distinct

# Convert rank to a factor
# df$rank <- as.character(df$rank)

# Compute the cumulative total for top25_share
# df <- df %>% group_by(year) %>% 
  # mutate(cumtotal = cumsum(total)) %>% 
  # ungroup 

# Create the plot
p <- ggplot(df_bar, aes(x = as.character(year), y = total, fill = rank, label = isocode)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked Bar Chart
  geom_text(position = position_stack(vjust = 0.5), color = "white") +  # Labels on the bar
  labs(x = "Year", y = "Amount received by the top 5 recipients") +
  # scale_fill_manual(values = c("1" = "red", "2" = "blue"), guide = "none") + # Manual colour assignment
  scale_y_continuous(
    # limits = c(0, 120)
    # ,
    sec.axis = sec_axis(~ ., name = "% of total received by the top 25 recipients", labels = scales::percent_format(scale = 0.8))) +  # Secondary axis
  # scale_y_continuous(limits = c(0, 120))+
  theme_minimal()

p <- p + geom_line(data = df_line, 
                   aes(x = as.character(year), 
                       y = share * 100/0.8, 
                       group = 1),
                   color = "green", 
                   size = 1)

print(p)


df_line <- df_crs_recipients %>% 
  select(year,share = top25_share) %>% 
  ungroup() %>% 
  distinct %>% 
  mutate(isocode = "World", rank = 6) 
# Create the plot
p <- ggplot() +
  geom_bar(data = df, aes(x = as.factor(year), y = total, fill = rank, label = isocode),
           stat = "identity", position = "stack") +  # Stacked Bar Chart
  geom_text(data = df, position = position_stack(vjust = 0.5), aes(x = as.factor(year), y = total, label = isocode), color = "white") +  # Labels on the bar
  labs(x = "Year", y = "Total") +
  # scale_fill_manual(values = c("1" = "red", "2" = "blue"), guide = "none") +  # Manual colour assignment
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Share", labels = percent_format(scale = 1.1))
  ) +  # Secondary axis as percent
  theme_minimal()

p <- ggplot() +
  geom_bar(data = df, aes(x = as.factor(year), y = total, fill = rank, label = isocode),
           stat = "identity", position = "stack") +  # Stacked Bar Chart
  geom_text(data = df, position = position_stack(vjust = 0.5), aes(x = as.factor(year), y = total, label = isocode), color = "white") +  # Labels on the bar
  labs(x = "Year", y = "Total") +
  # scale_fill_manual(values = c("1" = "red", "2" = "blue"), guide = "none") +  # Manual colour assignment
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Share", labels = percent_format(scale = 1))
  ) +  # Secondary axis as percent
  theme_minimal()

max_total <- max(df$total)
# Line Chart


