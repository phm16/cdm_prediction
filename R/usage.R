

# backing film ------------------------------------------------------------

train_df %>% 
  ggplot(., aes(x = TIMESTAMP, y = USAGE_OF_BACKING_FILM)) + 
  geom_line()

train_df %>% select(TIMESTAMP, USAGE_OF_BACKING_FILM) %>%
  mutate(
         LAG_USAGE_OF_BACKING_FILM = lag(USAGE_OF_BACKING_FILM, 1),
         LEAD_USAGE_OF_BACKING_FILM = lead(USAGE_OF_BACKING_FILM, 1),
         LAG_DIFF = USAGE_OF_BACKING_FILM - LAG_USAGE_OF_BACKING_FILM,
         LEAD_DIFF = LEAD_USAGE_OF_BACKING_FILM - USAGE_OF_BACKING_FILM,
         FLAG1 = if_else(LEAD_USAGE_OF_BACKING_FILM < USAGE_OF_BACKING_FILM, 1, 0),
         FLAG2 = if_else(USAGE_OF_BACKING_FILM == 0, 1, 0)
         ) %>%
  filter(LAG_DIFF < -100) %>%
  ggplot(., aes(x = LAG_DIFF)) + geom_histogram()
  
train_df %>% select(TIMESTAMP, USAGE_OF_BACKING_FILM) %>%
  mutate(
    LAG_USAGE_OF_BACKING_FILM = lag(USAGE_OF_BACKING_FILM, 1),
    LEAD_USAGE_OF_BACKING_FILM = lead(USAGE_OF_BACKING_FILM, 1),
    LAG_DIFF = USAGE_OF_BACKING_FILM - LAG_USAGE_OF_BACKING_FILM,
    LEAD_DIFF = LEAD_USAGE_OF_BACKING_FILM - USAGE_OF_BACKING_FILM,
    FLAG1 = if_else(LEAD_USAGE_OF_BACKING_FILM < USAGE_OF_BACKING_FILM, 1, 0),
    FLAG2 = if_else(USAGE_OF_BACKING_FILM == 0, 1, 0)
  ) %>%
  filter(TIMESTAMP > 481721804 & TIMESTAMP < 481731978)

train_df %>% select(TIMESTAMP, USAGE_OF_BACKING_FILM) %>%
  mutate(
    LAG_USAGE_OF_BACKING_FILM = lag(USAGE_OF_BACKING_FILM, 1),
    LEAD_USAGE_OF_BACKING_FILM = lead(USAGE_OF_BACKING_FILM, 1),
    LAG_DIFF = USAGE_OF_BACKING_FILM - LAG_USAGE_OF_BACKING_FILM,
    LEAD_DIFF = LEAD_USAGE_OF_BACKING_FILM - USAGE_OF_BACKING_FILM,
    FLAG1 = if_else(LEAD_USAGE_OF_BACKING_FILM < USAGE_OF_BACKING_FILM, 1, 0),
    FLAG2 = if_else(USAGE_OF_BACKING_FILM == 0, 1, 0)
  ) %>%
  filter(TIMESTAMP > (481634410 - 10000) & TIMESTAMP < (481634410 + 10000))



# usage data --------------------------------------------------------------

train_df %>% select(WAFER_ID, STAGE, TIMESTAMP, starts_with("USAGE")) %>%
  gather(var, val, -WAFER_ID, -STAGE, -TIMESTAMP) %>%
  ggplot(., aes(x = TIMESTAMP, y = val, color = var)) + geom_line() +
  facet_wrap(~ var, scales = "free_y")


# dresser -----------------------------------------------------------------

train_df %>% 
  ggplot(., aes(x = TIMESTAMP, y = USAGE_OF_DRESSER)) + 
  geom_line()

min_timestamp <- min(train_df$TIMESTAMP)
max_timestamp <- max(train_df$TIMESTAMP)
last_usage_of_dresser <- train_df %>% 
  summarize(LAST_USAGE_OF_DRESSER = last(USAGE_OF_DRESSER))

surv_df <- train_df %>% select(TIMESTAMP, USAGE_OF_DRESSER) %>%
  mutate(
    LAG_USAGE_OF_DRESSER = lag(USAGE_OF_DRESSER, 1),
    LEAD_USAGE_OF_DRESSER = lead(USAGE_OF_DRESSER, 1),
    LAG_DIFF = USAGE_OF_DRESSER - LAG_USAGE_OF_DRESSER,
    LEAD_DIFF = LEAD_USAGE_OF_DRESSER - USAGE_OF_DRESSER,
    FLAG1 = if_else(LAG_USAGE_OF_DRESSER > (USAGE_OF_DRESSER + 100), 1, 0),
    FLAG2 = if_else(USAGE_OF_DRESSER < 10, 1, 0)
  ) %>%
  filter(FLAG1 == 1) %>%
  select(USAGE_OF_DRESSER = LAG_USAGE_OF_DRESSER) %>%
  mutate(STATUS = 1) %>%
  bind_rows(tibble(USAGE_OF_DRESSER = last_usage_of_dresser, STATUS = 0))

survfit_mdl <- survfit(Surv(USAGE_OF_DRESSER, STATUS) ~ 1, data = surv_df)

GGally::ggsurv(survfit_mdl)
