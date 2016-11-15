# split models ------------------------------------------------------------

stations <- c("A123", "A456", "B456")

# split the validation set by station
validation_responses <- map(stations, function(station) {
  
  validation_df %>% 
    filter(STATION == station) %>%
    select(AVG_REMOVAL_RATE) %>%
    .$AVG_REMOVAL_RATE
  
}) %>% flatten_dbl(.)


# linear regression -------------------------------------------------------

glm_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        USAGE_OF_POLISHING_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc
      ),
    method = "glm",
    preProcess = c("zv"),
    trControl = ctrl
  )
  
})

glm_mdl_stations

# predictions
glm_station_pred <- map2(glm_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(glm_station_pred$pred, validation_responses)

# k-nearest neighbors -----------------------------------------------------

knn_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        USAGE_OF_POLISHING_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123
      ),
    method = "knn",
    preProcess = c("zv", "BoxCox"),
    trControl = ctrl,
    tuneGrid = data.frame(k = seq(1, 10, 1)),
    importance = TRUE
  )
  
})

# predictions
knn_station_pred <- map2(knn_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(knn_station_pred$pred, validation_responses)

# rpart -------------------------------------------------------------------

rpart_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        USAGE_OF_POLISHING_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc
      ),
    method = "rpart",
    preProcess = c("zv"),
    trControl = ctrl,
    tuneLength = 5
  )
  
})

rpart_mdl_stations

# tuning
map(rpart_mdl_stations, ggplot)

# variable importance
map(rpart_mdl_stations, function(mdl) varImp(mdl))
map(rpart_mdl_stations, function(mdl) ggplot(varImp(mdl)))

# predictions
rpart_station_pred <- map2(rpart_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(rpart_station_pred$pred, validation_responses)

# random forest -----------------------------------------------------------

rf_grid <- data.frame(mtry = seq(5, 50, 5))

rf_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df,
    method = "rf",
    preProcess = c("zv"),
    trControl = ctrl,
    tuneGrid = rf_grid,
    ntree = 100,
    importance = TRUE
  )
  
})

rf_mdl_stations

# tuning
map(rf_mdl_stations, ggplot)

# variable importance
map(rf_mdl_stations, function(mdl) varImp(mdl))
map(rf_mdl_stations, function(mdl) ggplot(varImp(mdl)))

# predictions
rf_station_pred <- map2(rf_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(rf_station_pred$pred, validation_responses)

# xgb ---------------------------------------------------------------------

xgb_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    #data = mdl_df %>% filter(STATION == station) %>% select(-STATION),
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        AVG_REMOVAL_RATE_lag1,
        AVG_REMOVAL_RATE_lag2,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        EDGE_AIR_BAG_PRESSURE_mean_P123,
        MAIN_OUTER_AIR_BAG_PRESSURE_mean_P123,
        PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
        SLURRY_FLOW_LINE_A_mean_P123,
        SLURRY_FLOW_LINE_B_mean_P123,
        SLURRY_FLOW_LINE_C_mean_P123,
        HEAD_ROTATION_mean_P123,
        WAFER_ROTATION_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc,
        EDGE_AIR_BAG_PRESSURE_auc,
        MAIN_OUTER_AIR_BAG_PRESSURE_auc,
        PRESSURIZED_CHAMBER_PRESSURE_auc,
        SLURRY_FLOW_LINE_A_auc,
        SLURRY_FLOW_LINE_B_auc,
        SLURRY_FLOW_LINE_C_auc,
        HEAD_ROTATION_auc,
        WAFER_ROTATION_auc
        
      ),
    method = "xgbTree",
    preProcess = "zv",
    trControl = ctrl,
    tuneLength = 4,
    importance = TRUE
  )
  
})

xgb_mdl_stations

# tuning
map(xgb_mdl_stations, ggplot)

# variable importance
map(xgb_mdl_stations, function(mdl) ggplot(varImp(mdl)))

# predictions
xgb_station_pred <- map2(xgb_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(xgb_station_pred$pred, validation_responses)


# svm ---------------------------------------------------------------------

svm_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        AVG_REMOVAL_RATE_lag1,
        AVG_REMOVAL_RATE_lag2,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        EDGE_AIR_BAG_PRESSURE_mean_P123,
        MAIN_OUTER_AIR_BAG_PRESSURE_mean_P123,
        PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
        SLURRY_FLOW_LINE_A_mean_P123,
        SLURRY_FLOW_LINE_B_mean_P123,
        SLURRY_FLOW_LINE_C_mean_P123,
        HEAD_ROTATION_mean_P123,
        WAFER_ROTATION_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc,
        EDGE_AIR_BAG_PRESSURE_auc,
        MAIN_OUTER_AIR_BAG_PRESSURE_auc,
        PRESSURIZED_CHAMBER_PRESSURE_auc,
        SLURRY_FLOW_LINE_A_auc,
        SLURRY_FLOW_LINE_B_auc,
        SLURRY_FLOW_LINE_C_auc,
        HEAD_ROTATION_auc,
        WAFER_ROTATION_auc
        
      ),
    method = "svmRadial",
    preProcess = "zv",
    trControl = ctrl,
    tuneLength = 5
  )
  
})

map(svm_mdl_stations, varImp)

# predictions
svm_station_pred <- map2(svm_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(svm_station_pred$pred, validation_responses)

svm_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        USAGE_OF_POLISHING_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc
      ),
    method = "svmRadial",
    preProcess = "zv",
    trControl = ctrl,
    tuneLength = 5
  )
  
})

map(svm_mdl_stations, varImp)

# predictions
svm_station_pred <- map2(svm_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(svm_station_pred$pred, validation_responses)

# glmnet ------------

glmnet_mdl_stations <- map(stations, function(station) {
  
  set.seed(59)
  train(
    AVG_REMOVAL_RATE ~ .,
    data = mdl_df %>% filter(STATION == station) %>%
      select(
        AVG_REMOVAL_RATE,
        AVG_REMOVAL_RATE_lag1,
        AVG_REMOVAL_RATE_lag2,
        USAGE_OF_DRESSER_max_P123,
        USAGE_OF_BACKING_FILM_max_P123,
        USAGE_OF_DRESSER_TABLE_max_P123,
        CENTER_AIR_BAG_PRESSURE_mean_P123,
        EDGE_AIR_BAG_PRESSURE_mean_P123,
        MAIN_OUTER_AIR_BAG_PRESSURE_mean_P123,
        PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
        SLURRY_FLOW_LINE_A_mean_P123,
        SLURRY_FLOW_LINE_B_mean_P123,
        SLURRY_FLOW_LINE_C_mean_P123,
        HEAD_ROTATION_mean_P123,
        WAFER_ROTATION_mean_P123,
        CENTER_AIR_BAG_PRESSURE_auc,
        EDGE_AIR_BAG_PRESSURE_auc,
        MAIN_OUTER_AIR_BAG_PRESSURE_auc,
        PRESSURIZED_CHAMBER_PRESSURE_auc,
        SLURRY_FLOW_LINE_A_auc,
        SLURRY_FLOW_LINE_B_auc,
        SLURRY_FLOW_LINE_C_auc,
        HEAD_ROTATION_auc,
        WAFER_ROTATION_auc
      ),
    preProcess = c("zv", "nzv", "BoxCox"),
    method = "earth",
    trControl = ctrl,
    tuneLength = 5
  )
  
})

glmnet_mdl_stations

map(glmnet_mdl_stations, varImp)

# predictions
glmnet_station_pred <- map2(glmnet_mdl_stations, stations, function(mdl, station) {
  data.frame(pred = predict(mdl, validation_df %>% filter(STATION == station))) %>% mutate(station = station)
}) %>% map_df(., data.frame)

# validation set performance
postResample(glmnet_station_pred$pred, validation_responses)