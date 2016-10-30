
# boxplot of avg removal rate by sequence
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = SEQUENCE, y = AVG_REMOVAL_RATE)) + 
  geom_boxplot()

# time series plot of removal rate vs. time by station
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TIMESTAMP_mean, 
           y = AVG_REMOVAL_RATE
       )
) + geom_line() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm") +
  labs(x = "TIMESTAMP")
  #ggtitle("Removal Rate by Processing Station") #+
  #ggsave("figures/removal_rate_by_station.png", device = "png", width = 6, height = 4)