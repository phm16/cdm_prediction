

# plot usage data ---------------------------------------------------------

# plot all usage by facets
base_df %>% select(WAFER_ID, STAGE, STATION, TIMESTAMP, starts_with("USAGE")) %>%
  gather(var, val, -WAFER_ID, -STAGE, -STATION, -TIMESTAMP) %>%
  ggplot(., aes(x = TIMESTAMP, y = val, color = STATION)) + geom_line() +
  facet_wrap(~ var, scales = "free_y", nrow = 6) +
  theme(legend.position = "top") +
  ggsave("figures/usage_by_station.png", device = "png", width = 6, height = 8)

# plot usage individually
base_df %>% 
  select(TIMESTAMP, STATION, USAGE_OF_BACKING_FILM) %>%
  mutate(USAGE_OF_BACKING_FILM_LAG = lag(USAGE_OF_BACKING_FILM, 1L),
         LAG_FLAG = if_else(USAGE_OF_BACKING_FILM == USAGE_OF_BACKING_FILM_LAG, 1L, 0L)
  ) %>%
  filter(LAG_FLAG == 0L) %>%
  ggplot(., aes(x = TIMESTAMP, y = USAGE_OF_BACKING_FILM, color = STATION)) +
  geom_line() +
  facet_wrap(~ STATION, nrow = 3) +
  ggsave("figures/usage_of_backing_film.png", device = "png", width = 6, height = 4)

  ggplotly()
  