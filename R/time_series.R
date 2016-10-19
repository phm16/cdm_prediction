

ggplot(train_df_long %>% filter(WAFER_ID == 2939014292, STAGE == "A"),
       aes(x = TIMESTAMP, y = val, color = as.factor(CHAMBER))) +
  geom_line() +
  facet_wrap(~ var, scales = "free") +
  #ggtitle("WAFER_ID: 371447024, STAGE: A") +
  theme_grey(base_size = 8)

arima_mdl <- train_df %>% filter(WAFER_ID == 2939014292, STAGE == "A") %>%
  select(TIMESTAMP, CENTER_AIR_BAG_PRESSURE) %>%
  .$CENTER_AIR_BAG_PRESSURE %>%
  auto.arima(.)

plot(forecast(arima_mdl, h = 200))

arima_mdl <- train_df %>% 
  #filter(WAFER_ID == 2939014292, STAGE == "A") %>%
  select(TIMESTAMP, USAGE_OF_DRESSER) %>%
  .$USAGE_OF_DRESSER %>%
  auto.arima(.)

plot(forecast(arima_mdl, h = 20000))

train_df %>% filter(WAFER_ID == 2939014292, STAGE == "A") %>%
  ggplot(., aes(x = TIMESTAMP, y = WAFER_ROTATION)) + geom_line()

  


