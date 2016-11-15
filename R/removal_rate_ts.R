

tr <- train_df_ts %>% select(TIMESTAMP, AVG_REMOVAL_RATE, STATION, contains("USAGE")) %>% mutate(SET = "TRAINING")
vl <- validation_df_ts %>% select(TIMESTAMP, AVG_REMOVAL_RATE, STATION, contains("USAGE")) %>% mutate(SET = "VALIDATION")
test <- test_df_ts %>% select(TIMESTAMP, AVG_REMOVAL_RATE, STATION, contains("USAGE")) %>% mutate(SET = "TEST")

all <- tr %>% bind_rows(vl) %>% bind_rows(test)

glimpse(all)

ggplot(all %>% 
         sample_frac(0.01) %>%
         #filter(TIMESTAMP < 4.82E8) %>%
         filter(AVG_REMOVAL_RATE < 4000), 
       aes(x = TIMESTAMP, y = AVG_REMOVAL_RATE, color = STATION)) +
  geom_line() +
  geom_point(shape = 1) +
  facet_wrap(~ STATION, nrow = 3)

all %>% 
  sample_frac(0.01) %>%
  select(-AVG_REMOVAL_RATE, -SET) %>%
  gather(var, val, -TIMESTAMP, -STATION) %>%
  ggplot(., aes(x = TIMESTAMP, y = val, color = STATION)) + 
  geom_line() +
  facet_wrap(~ var, nrow = 6, scales = "free_y")


