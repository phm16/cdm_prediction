

# timestamps per wafer / stage
base_df %>% count(WAFER_ID, STAGE) %>%
  ggplot(., aes(x = n)) +
  geom_histogram() +
  lims(x = c(0, 500))

# show the distribution of the duration between timestamps

# max 10 gap
base_df %>% select(TIMESTAMP) %>%
  arrange(TIMESTAMP) %>%
  mutate(PREV_TIMESTAMP = lag(TIMESTAMP, 1),
         DIFF = TIMESTAMP - PREV_TIMESTAMP) %>%
  filter(DIFF < 10, DIFF > -10) %>%
  ggplot(., aes(x = DIFF)) + geom_histogram()

# max 1 gap
base_df %>% select(TIMESTAMP, STATION) %>%
  arrange(TIMESTAMP) %>%
  mutate(PREV_TIMESTAMP = lag(TIMESTAMP, 1),
         DIFF = TIMESTAMP - PREV_TIMESTAMP) %>%
  filter(DIFF < 10, DIFF > -10) %>%
  ggplot(., aes(x = DIFF, fill = STATION)) + geom_histogram(position = "dodge")

# time series
base_df %>% select(TIMESTAMP, STAGE, CHAMBER) %>%
  unite(STAGE_CHAMBER, STAGE, CHAMBER) %>%
  arrange(TIMESTAMP) %>%
  mutate(PREV_TIMESTAMP = lag(TIMESTAMP, 1),
         DIFF = TIMESTAMP - PREV_TIMESTAMP,
         JUMP_FLAG = if_else(DIFF > 1, "JUMP", "NORMAL")
  ) %>%
  #filter(DIFF < 1, DIFF > - 1) %>%
  ggplot(., aes(x = TIMESTAMP, y = DIFF, color = JUMP_FLAG)) +
  geom_point() +
  facet_wrap(~ STAGE_CHAMBER)