

# plot usage data ---------------------------------------------------------

# plot all usage by facets
train_df %>% select(WAFER_ID, STAGE, TIMESTAMP, starts_with("USAGE")) %>%
  gather(var, val, -WAFER_ID, -STAGE, -TIMESTAMP) %>%
  ggplot(., aes(x = TIMESTAMP, y = val, color = var)) + geom_line() +
  facet_wrap(~ var, scales = "free_y")

# plot usage individually
train_df %>% select(TIMESTAMP, USAGE_OF_BACKING_FILM) %>%
  ggplot(., aes(x = TIMESTAMP, y = USAGE_OF_BACKING_FILM)) +
  geom_point()

