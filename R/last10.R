slices <- seq(min(train_df_ts$TIMESTAMP), max(train_df_ts$TIMESTAMP), 1000)

slices <- seq(1, nrow(train_df_ts), 1000)


train_df_ts %>% select(WAFER_ID, STAGE, STATION, TIMESTAMP, contains("USAGE")) %>%
  slice(slices) %>%
  gather(var, val, -WAFER_ID, -STAGE, -STATION, -TIMESTAMP) %>%
  ggplot(., aes(x = TIMESTAMP, y = val, color = STATION)) + 
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~ var, scales = "free_y", nrow = 6) +
  theme(legend.position = "top") 



# last 10 jobs ------------------------------------------------------------

last_10_wafer_id <- train_df_ts %>%
  filter(STATION %in% c("A456", "B456"),
         CHAMBER == 4) %>%
  select(WAFER_ID, TIMESTAMP) %>%
  group_by(WAFER_ID) %>%
  summarize(TIMESTAMP_max = max(TIMESTAMP)) %>%
  ungroup() %>%
  arrange(desc(TIMESTAMP_max)) %>%
  top_n(10, TIMESTAMP_max) %>%
  .$WAFER_ID

train_df_ts_last_10_jobs <- train_df_ts %>% filter(WAFER_ID %in% last_10_wafer_id)

# write_rds(train_df_ts_last_10_jobs, "data/train_df_ts_last_10_jobs.rds")

ggplot(train_df_ts %>% 
         filter(WAFER_ID == -4224160600, STAGE == "A", CHAMBER == 4L),
       aes(x = TIMESTAMP, y = CENTER_AIR_BAG_PRESSURE)) +
  geom_line()

ggplot(train_df_ts %>% 
         filter(WAFER_ID == -4224160600, STAGE == "A", CHAMBER == 4L),
       aes(x = TIMESTAMP, y = PRESSURIZED_CHAMBER_PRESSURE)) +
  geom_line()

ggplot(train_df_ts %>% 
         filter(WAFER_ID == -4224160600, STAGE == "A", CHAMBER == 4L),
       aes(x = TIMESTAMP, y = SLURRY_FLOW_LINE_A)) +
  geom_line()

ggplot(train_df_ts %>% 
         filter(WAFER_ID == -4224160600, STAGE == "A", CHAMBER == 4L),
       aes(x = TIMESTAMP, y = SLURRY_FLOW_LINE_C)) +
  geom_line()