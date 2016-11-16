

# ensemble ----------------------------------------------------------------

mdls <- list(
  knn = knn_mdl_stations,
  rf = rf_mdl_stations,
  xgb = xgb_mdl_stations,
  svm = svm_mdl_stations
)

ensemble_weights <- mdls %>% 
  map(function(mdl) map_dbl(mdl, function(mdl) min(mdl$results$RMSE))) %>% 
  data.frame(.) %>%
  mutate(station = c("A123", "A456", "B456")) %>%
  gather(mdl, RMSE, -station) %>%
  group_by(station) %>%
  mutate(MSE = RMSE ^ 2, INV_RMSE = 1 / RMSE) %>%
  ungroup() %>%
  group_by(station) %>%
  dplyr::mutate(total = sum(INV_RMSE), weight = INV_RMSE / total) %>%
  ungroup() %>%
  arrange(station) %>%
  select(station, mdl, weight)

data.frame(
  list(
    knn = knn_station_pred,
    rf = rf_station_pred, 
    xgb = xgb_station_pred, 
    svm = svm_station_pred
  )) %>%
  gather(mdl, prediction, -station)

predictions <- knn_station_pred %>% mutate(mdl = "knn", obs = row_number()) %>% bind_rows(
  rf_station_pred %>% mutate(mdl = "rf", obs = row_number())) %>% bind_rows(
    xgb_station_pred %>% mutate(mdl = "xgb", obs = row_number())) %>% bind_rows(
      svm_station_pred %>% mutate(mdl = "svm", obs = row_number()))

ensemble_predictions <- predictions %>% inner_join(ensemble_weights, by = c("station", "mdl")) %>%
  group_by(obs) %>%
  summarize(pred = weighted.mean(pred, weight)) %>%
  .$pred

# validation set performance
postResample(ensemble_predictions, validation_responses)
