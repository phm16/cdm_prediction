train <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/train_df2.csv")
test <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/test_df.csv")


#train$AVG_REMOVAL_RATE_OUTLIER_FLAG <-  mutate(train,AVG_REMOVAL_RATE_OUTLIER_FLAG  = ifelse(AVG_REMOVAL_RATE_OUTLIER_FLAG ==0, "No", "Yes"))


head(train$AVG_REMOVAL_RATE_OUTLIER_FLAG)

# Outliers left in training set-----------------------------

sparse_matrix_ol = sparse.model.matrix(AVG_REMOVAL_RATE_OUTLIER_FLAG ~ .-WAFER_ID -STAGE -AVG_REMOVAL_RATE -SEQUENCE -1, data = train)
output_vector_ol=train[,"AVG_REMOVAL_RATE_OUTLIER_FLAG"]

# Levels are needed when using factors in caret for classification
y <- as.factor(train[,4])
levels(y) <-  c("No", "Yes")


# set up the cross-validated hyper-parameter search
xgb_grid_ol = expand.grid(
  nrounds = c(1000,2000),
  eta = c(0.01, 0.001),
  max_depth = c(4,5,6,7),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1
)

# pack the training control parameters
xgb_trcontrol_ol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  #twoClassSummary allows ROC to be used as performance metric, default is accuracy
  #summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  savePredictions = TRUE
 )

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
set.seed(123)
xgb_train_ol = train(
  x = sparse_matrix_ol,
  y = y,
  trControl = xgb_trcontrol_ol,
  tuneGrid = xgb_grid_ol,
  method = "xgbTree",
  objective = "binary:logistic",
  nthread = 8
)

xgb_train_ol
plot(xgb_train_ol)

# Run model on test data

sparse_matrix_test_ol = sparse.model.matrix(AVG_REMOVAL_RATE_OUTLIER_FLAG ~ .-WAFER_ID -STAGE -SEQUENCE -1, data = test)
xgb_pred <- predict(xgb_train_ol, newdata = sparse_matrix_test_ol)


# Create levels in test data so a confusion matrix can be constructed
levels(test$AVG_REMOVAL_RATE_OUTLIER_FLAG) <-  c("No", "Yes")
table(xgb_pred,test$AVG_REMOVAL_RATE_OUTLIER_FLAG)

# Use validation function for accuracy, precision, etc

validate(test$AVG_REMOVAL_RATE_OUTLIER_FLAG,xgb_pred)

# Gradient Boosting Variable Important
GB <- varImp(xgb_train_ol)
GB


# Roc curve is a work in progress ----------------------------------------------
#Draw the ROC curve from max kuhn caret example

gbm.probs <- predict(xgb_train_ol, newdata = sparse_matrix_test_ol, type = 'prob')
head(gbm.probs)


gbmROC <- roc(test$AVG_REMOVAL_RATE_OUTLIER_FLAG, gbm.probs[, "No"])


plot(gbmROC, type = "S", main = "GBM ROC Curve - AUC 0.9756")


