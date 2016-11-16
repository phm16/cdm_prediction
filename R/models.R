


# null model --------------------------------------------------------------

set.seed(59)
glm_null <- train(
  AVG_REMOVAL_RATE ~ 1,
  data = train_df,
  method = "glm",
  trControl = ctrl
)

glm_null

# glm by station ----------------------------------------------------------

set.seed(59)
glm_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ STATION,
  data = train_df,
  method = "glm",
  trControl = ctrl
)

summary(glm_mdl_1$finalModel)

glm_mdl_1

# predictions
glm_mdl_1_pred <- predict(glm_mdl_1, validation_df)

# validation set performance
postResample(glm_mdl_1_pred, validation_df$AVG_REMOVAL_RATE)

# glm random forest vars --------------------------------------------------

set.seed(59)
glm_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df %>%
    select(
      AVG_REMOVAL_RATE,
      AVG_REMOVAL_RATE_lag1,
      STATION,
      CENTER_AIR_BAG_PRESSURE_mean_P123,
      PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
      SLURRY_FLOW_LINE_A_mean_P123,
      SLURRY_FLOW_LINE_C_mean_P123,
      USAGE_OF_DRESSER_max_P123,
      USAGE_OF_DRESSER_TABLE_max_P123,
      HEAD_ROTATION_mean_P123,
      WAFER_ROTATION_mean_P123
    ),
  method = "glm",
  trControl = ctrl
)

summary(glm_mdl_2$finalModel)

glm_mdl_2

# predictions
glm_mdl_2_pred <- predict(glm_mdl_2, validation_df)

# validation set performance
postResample(glm_mdl_2_pred, validation_df$AVG_REMOVAL_RATE)

# rpart -------------------------------------------------------------------

set.seed(59)
rpart_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 5
)

rpart_mdl_1

# prp tree plot
rpart.plot::prp(rpart_mdl_1$finalModel)

# rattle tree plot
rattle::fancyRpartPlot(rpart_mdl_1$finalModel, main = "", sub = "")

# variable importance
ggplot(varImp(rpart_mdl_1))

# predictions
rpart_pred <- predict(rpart_mdl_1, validation_df)

# validation set performance
postResample(rpart_pred, validation_df$AVG_REMOVAL_RATE)

# reduced set
set.seed(59)
rpart_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df %>%
    select(
      AVG_REMOVAL_RATE,
      AVG_REMOVAL_RATE_lag1,
      STATION,
      CENTER_AIR_BAG_PRESSURE_mean_P123,
      PRESSURIZED_CHAMBER_PRESSURE_max_P123,
      SLURRY_FLOW_LINE_A_mean_P123,
      USAGE_OF_DRESSER_max_P123,
      USAGE_OF_DRESSER_TABLE_max_P123,
      HEAD_ROTATION_mean_P123,
      WAFER_ROTATION_mean_P123
    ),
  method = "rpart",
  tuneLength = 10,
  trControl = ctrl
)

summary(rpart_mdl_2$finalModel)

rpart_mdl_2

# predictions
rpart_mdl_2_pred <- predict(rpart_mdl_2, validation_df)

# validation set performance
postResample(rpart_mdl_2_pred, validation_df$AVG_REMOVAL_RATE)

# random forest -----------------------------------------------------------

rf_grid <- data.frame(mtry = seq(3, 30, 3))

# random forest 
set.seed(59)
rf_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df,
  method = "rf",
  preProcess = "zv",
  trControl = ctrl,
  tuneGrid = rf_grid,
  ntree = 100,
  importance = TRUE
)

rf_mdl_1

# tuning
ggplot(rf_mdl_1)

# variable importance
ggplot(varImp(rf_mdl_1))

rf_varimp <- data.frame(variable = dimnames(varImp(rf_mdl_1)$importance)[[1]], 
                        importance = unname(varImp(rf_mdl_1)$importance[, 1])
) %>% arrange(desc(importance)) %>% .[1:20, ]

ggplot(rf_varimp, aes(x = fct_reorder(variable, importance), y = importance)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Feature")

# predictions
rf_pred_1 <- predict(rf_mdl_1, validation_df)

# validation set performance
postResample(rf_pred_1, validation_df$AVG_REMOVAL_RATE)

# xgb ---------------------------------------------------------------------

set.seed(59)
xgb_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df,
  method = "xgbTree",
  preProcess = "zv",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

xgb_mdl_1

ggplot(xgb_mdl_1)

ggplot(varImp(xgb_mdl_1))

# predictions
xgb_pred_1 <- predict(xgb_mdl_1, validation_df)

# validation set performance
postResample(xgb_pred_1, validation_df$AVG_REMOVAL_RATE)

# glmnet ------------------------------------------------------------------

set.seed(59)
glmnet_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = mdl_df,
  method = "glmnet",
  preProcess = "zv",
  trControl = ctrl,
  tuneLength = 5
)

glmnet_mdl_1 

ggplot(glmnet_mdl_1)

ggplot(varImp(glmnet_mdl_1))