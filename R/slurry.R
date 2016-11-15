


ggplot(
  train_df %>% 
    filter(AVG_REMOVAL_RATE < 4000,
           SLURRY_FLOW_LINE_A_sum < 5000),
  aes(x = SLURRY_FLOW_LINE_A_sum, y = AVG_REMOVAL_RATE)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STATION)

ggplot(
  train_df %>% 
    filter(AVG_REMOVAL_RATE < 4000,
           SLURRY_FLOW_LINE_A_sum < 5000,
           SLURRY_FLOW_LINE_A_mean * TOTAL_POLISHING_DURATION < 10000),
  aes(x = SLURRY_FLOW_LINE_A_mean * TOTAL_POLISHING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STATION)


ggplot(
  train_df %>% 
    filter(AVG_REMOVAL_RATE < 4000,
           SLURRY_FLOW_LINE_A_sum < 5000,
           SLURRY_FLOW_LINE_A_mean * TOTAL_POLISHING_DURATION < 10000),
  aes(x = TOTAL_POLISHING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STATION)

ggplot(
  train_df %>% 
    filter(AVG_REMOVAL_RATE < 4000,
           SLURRY_FLOW_LINE_A_sum < 5000,
           SLURRY_FLOW_LINE_A_mean * TOTAL_POLISHING_DURATION < 10000),
  aes(x = A_3, y = AVG_REMOVAL_RATE)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STATION)

ggplot(
  train_df %>% 
    filter(AVG_REMOVAL_RATE < 4000,
           SLURRY_FLOW_LINE_A_sum < 5000,
           SLURRY_FLOW_LINE_A_mean * TOTAL_POLISHING_DURATION < 10000),
  aes(x = USAGE_OF_BACKING_FILM_max, y = AVG_REMOVAL_RATE)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STATION) +
  geom_smooth()