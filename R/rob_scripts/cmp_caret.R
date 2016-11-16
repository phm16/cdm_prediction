# train and validation from Zach's script, not my data munging script--------------------------------



train <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/train_df.csv")
validation <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP VALIDATION DATA SET/2016 PHM DATA CHALLENGE CMP VALIDATION DATA SET/validation_df.csv")



# Outliers removed from training set ----------------------
train_df <- train %>% filter(AVG_REMOVAL_RATE < 1000)

sparse_matrix_trn = sparse.model.matrix(AVG_REMOVAL_RATE ~ .-WAFER_ID -SEQUENCE -1, data = train_df)
output_vector_trn=train_df[,"AVG_REMOVAL_RATE"]



# pack the training control parameters
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  allowParallel = TRUE
)


# train the model for each parameter combination in the grid, 
# using CV to evaluate ouliers removed from dataset

xgb_grid = expand.grid(
  nrounds = c(500,1000,2000),
  eta = c(0.1,0.01, 0.001,0.0001,0.00001),
  max_depth = c(5,6,7,8,9,10,11,12,24,32,64),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1
)


##### XGB Model

set.seed(123)
xgb_train = train(
  x = sparse_matrix_trn, # Outliers removed
  y = output_vector_trn,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  objective = "reg:linear"
)


# Outliers removed
xgb_train

# gradient boosting variable importance
xgb_imp <- varImp(xgb_train, scale = TRUE)
xgb_imp

# Plot model performance

ggplot(xgb_train) + ggtitle( "GBM Parameter Tuning")

# predict on validation data set using model trained without outliers

sparse_matrix_val = sparse.model.matrix(AVG_REMOVAL_RATE ~ .-WAFER_ID -STAGE -1, data = validation)
xgb_preds <- predict(xgb_train, newdata = sparse_matrix_val) #Outliers removed
gb_rmse <- sqrt(mean((xgb_preds - validation$AVG_REMOVAL_RATE)^2))
gb_rmse




# Random Forest ------------------------------------------------------------

big_formula <- AVG_REMOVAL_RATE ~ .-WAFER_ID -SEQUENCE


rf_grid = expand.grid(
  mtry = c(2,4,8,15,32,62,79,109,128)
)



ctrl = trainControl(
  method = "cv",
  number = 5,
 # save losses across all models
  allowParallel = TRUE,
  savePredictions = TRUE
)

# RF Outliers Removed --------------------------------------------
set.seed(123)
rf_mdl_2 <- train(
  form = big_formula,
  data = train_df,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rf_grid,
  ntree = 1001,
  importance = TRUE
 )

rf_mdl_2

ggplot(rf_mdl_2) + ggtitle("Random Forest Parameter Tuning")

rf_imp <- varImp(rf_mdl_2)
rf_imp

rf_rr_preds <- predict(rf_mdl_2, newdata = validation)
rf_rmse <- sqrt(mean((rf_rr_preds - validation$AVG_REMOVAL_RATE)^2))
rf_rmse


