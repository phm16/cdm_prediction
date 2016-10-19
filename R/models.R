# exploratory models ------------------------------------------------------

ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, savePredictions = "final")

# glm
glm_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ SEQUENCE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "glm",
  trControl = ctrl
)

summary(glm_mdl_1$finalModel)

glm_mdl_1

# rpart
rpart_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

rpart_mdl_1

rpart.plot::prp(rpart_mdl_1$finalModel)

rattle::fancyRpartPlot(rpart_mdl_1$finalModel, main = "", sub = "")

ggplot(varImp(rpart_mdl_1))

rpart_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

rpart_mdl_2

rpart.plot::prp(rpart_mdl_2$finalModel)

rattle::fancyRpartPlot(rpart_mdl_2$finalModel, main = "", sub = "")

ggplot(varImp(rpart_mdl_2))

# random forest

rf_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,
  ntree = 100,
  importance = TRUE
)

rf_mdl_1

ggplot(rf_mdl_1)

ggplot(varImp(rf_mdl_1))

rf_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,
  ntree = 100,
  importance = TRUE
)

rf_mdl_2

ggplot(rf_mdl_2)

ggplot(varImp(rf_mdl_2))

# xgb tree
xgb_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

xgb_mdl_1

ggplot(xgb_mdl_1)

ggplot(varImp(xgb_mdl_1))


# high-dimension models ---------------------------------------------------

# rpart
rpart_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rpart",
  trControl = ctrl,
  tuneLength = 5
)

rpart_mdl_1

rpart.plot::prp(rpart_mdl_1$finalModel)

rattle::fancyRpartPlot(rpart_mdl_1$finalModel, main = "", sub = "")

ggplot(varImp(rpart_mdl_1))

# random forest
rf_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,
  ntree = 100,
  importance = TRUE
)

rf_mdl_1

ggplot(rf_mdl_1)

ggplot(varImp(rf_mdl_1))

# xgb tree
xgb_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

xgb_mdl_1

ggplot(xgb_mdl_1)

ggplot(varImp(xgb_mdl_1))



# medium-dimension models -------------------------------------------------

mdl_formula <- as.formula(
  AVG_REMOVAL_RATE ~
    A_1 +
    A_2 +
    A_3 +
    A_4 +
    A_5 +
    A_6 +
    B_4 +
    B_5 +
    B_6 #+
    # SEQUENCE +
    # USAGE_OF_DRESSER_TABLE_min +
    # USAGE_OF_DRESSER_TABLE_mean +
    # USAGE_OF_DRESSER_TABLE_max +
    # CENTER_AIR_BAG_PRESSURE_min +
    # CENTER_AIR_BAG_PRESSURE_mean +
    # CENTER_AIR_BAG_PRESSURE_max +
    # HEAD_ROTATION_min +
    # HEAD_ROTATION_mean +
    # HEAD_ROTATION_max
)

# rpart
rpart_mdl_2 <- train(
  form = mdl_formula,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

rpart_mdl_2

rpart.plot::prp(rpart_mdl_2$finalModel)

rattle::fancyRpartPlot(rpart_mdl_2$finalModel, main = "", sub = "")

ggplot(varImp(rpart_mdl_2))

# random forest
rf_mdl_2 <- train(
  form = mdl_formula,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,
  ntree = 500,
  importance = TRUE
)

rf_mdl_2

ggplot(rf_mdl_2)

ggplot(varImp(rf_mdl_2))

# xgb tree
xgb_mdl_2 <- train(
  form = mdl_formula,
  data = train_df_mdl %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

xgb_mdl_2

ggplot(xgb_mdl_2)

ggplot(varImp(xgb_mdl_2))


# outlier classifier ------------------------------------------------------

ctrl <- trainControl(method = "boot632", number = 5, allowParallel = TRUE, savePredictions = "final")

rpart_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df_mdl,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

rpart_outlier

# random forest
rf_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df_mdl,
  method = "rf",
  trControl = ctrl,
  tuneLength = 10,
  ntree = 200,
  importance = TRUE
)

rf_outlier

ggplot(varImp(rf_outlier))

rpart_outlier2 <- train(
  OUTLIER ~ USAGE_OF_MEMBRANE_max + USAGE_OF_BACKING_FILM_max + USAGE_OF_PRESSURIZED_SHEET_max,
  data = train_df_mdl,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 5
)

rpart_outlier2

# xgb tree
xgb_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df_mdl,
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 3,
  importance = TRUE
)

xgb_outlier

ggplot(xgb_outlier)

ggplot(varImp(xgb_outlier))

ggplot(train_df_mdl, aes(x = OUTLIER, y = USAGE_OF_BACKING_FILM_max)) + geom_boxplot()

ggplot(train_df_mdl, aes(x = OUTLIER, y = TOTAL_PROCESSING_DURATION)) + geom_boxplot()
