

# model formulas ----------------------------------------------------------

mdl_formula1 <- as.formula(AVG_REMOVAL_RATE ~ 
  SEQUENCE +
  TIMESTAMP_max +
  STAGE_ROTATION_mean +
  WAFER_ROTATION_mean +
  SLURRY_FLOW_LINE_A_mean +
  SLURRY_FLOW_LINE_B_mean +
  SLURRY_FLOW_LINE_C_mean +
  USAGE_OF_BACKING_FILM_max +
  USAGE_OF_DRESSER_max +
  USAGE_OF_MEMBRANE_max +
  USAGE_OF_POLISHING_TABLE_max +
  USAGE_OF_PRESSURIZED_SHEET_max
)


mdl_formula2 <- as.formula(
  AVG_REMOVAL_RATE ~
    TIMESTAMP_max +
    STAGE_ROTATION_mean +
    WAFER_ROTATION_mean +
    SLURRY_FLOW_LINE_A_mean +
    SLURRY_FLOW_LINE_B_mean +
    SLURRY_FLOW_LINE_C_mean +
    USAGE_OF_BACKING_FILM_max +
    USAGE_OF_DRESSER_max +
    USAGE_OF_MEMBRANE_max +
    USAGE_OF_POLISHING_TABLE_max +
    USAGE_OF_PRESSURIZED_SHEET_max
)


# train control -----------------------------------------------------------

ctrl <-
  trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    savePredictions = "final"
  )

# exploratory models ------------------------------------------------------

# glm
glm_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ SEQUENCE,
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "glm",
  trControl = ctrl
)

summary(glm_mdl_1$finalModel)

glm_mdl_1

# rpart
set.seed(59)
rpart_mdl_1 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE -SEQUENCE -TIMESTAMP_min,
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000),
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
  #tuneGrid = data.frame(cp = c(1E-2, 1E-1))
)

rpart_mdl_1

rpart.plot::prp(rpart_mdl_1$finalModel)

rattle::fancyRpartPlot(rpart_mdl_1$finalModel, main = "", sub = "")

ggplot(varImp(rpart_mdl_1))

rpart_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE,
  data = train_df,
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
  AVG_REMOVAL_RATE ~ . -WAFER_ID -STAGE -SEQUENCE -TIMESTAMP_min,
  #mdl_formula1,
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000),
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
  data = train_df,
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
  mdl_formula1 - STATION,
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000, STATION == "A123"),
  method = "xgbTree",
  preProcess = "zv",
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
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000),
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
  data = train_df %>% filter(AVG_REMOVAL_RATE < 1000),
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
xgb_mdl2 <- train(
  mdl_formula2,
  data = train_df %>% select(
    -STAGE,
    -ends_with("min"), -A_1, -A_2, -A_3, -B_4, -B_5, -B_6) %>% 
    filter(AVG_REMOVAL_RATE < 1000, STATION == "A456"),
  method = "xgbTree",
  preProcess = "zv",
  trControl = ctrl,
  tuneLength = 5,
  importance = TRUE
)

xgb_mdl2

ggplot(xgb_mdl2)

ggplot(varImp(xgb_mdl2))

# outlier classifier ------------------------------------------------------

ctrl <- trainControl(method = "boot632", number = 5, allowParallel = TRUE, savePredictions = "final")

rpart_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 10
)

rpart_outlier

# random forest
rf_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df,
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
  data = train_df,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 5
)

rpart_outlier2

# xgb tree
xgb_outlier <- train(
  OUTLIER ~ . - AVG_REMOVAL_RATE,
  data = train_df,
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 3,
  importance = TRUE
)

xgb_outlier

ggplot(xgb_outlier)

ggplot(varImp(xgb_outlier))

ggplot(train_df, aes(x = OUTLIER, y = USAGE_OF_BACKING_FILM_max)) + geom_boxplot()

ggplot(train_df, aes(x = OUTLIER, y = TOTAL_PROCESSING_DURATION)) + geom_boxplot()
