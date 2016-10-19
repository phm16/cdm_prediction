
train_df_mdl <- features %>% mutate(OUTLIER = if_else(AVG_REMOVAL_RATE > 1000, "OUTLIER", "NORMAL"))

# write_csv(train_df_mdl, "data/phm_train_20161012.csv")

#%>% select(-ends_with("min"))

ggplot(train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = SEQUENCE, y = AVG_REMOVAL_RATE)) + 
  geom_boxplot()


ggplot(train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TIMESTAMP_max, 
           y = AVG_REMOVAL_RATE
           )
       ) + geom_line() +
  facet_wrap(~ SEQUENCE) +
  geom_smooth(method = "lm") +
  labs(x = "TIMESTAMP") +
  ggtitle("Removal Rate by Processing Sequence") +
  ggsave("figures/removal_rate_by_sequence.png", device = "png", width = 6, height = 4)
  