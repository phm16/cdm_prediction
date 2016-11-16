library(DMwR)

train <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/train_df2.csv")
test <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/test_df.csv")


sData <- train[,-c(1,2,3)]
as.factor 


sData$AVG_REMOVAL_RATE_OUTLIER_FLAG <- as.factor(sData$AVG_REMOVAL_RATE_OUTLIER_FLAG)


# Since there are only 3 outlier observations, k must be set to 2 to avoid missing values
trainSmote <- SMOTE(AVG_REMOVAL_RATE_OUTLIER_FLAG ~ ., sData, k= 2, perc.over = 20000, perc.under = 25)
 
prop.table(table(trainSmote$AVG_REMOVAL_RATE_OUTLIER_FLAG))


#### GBM using SMOTE Data Set


sparse_matrix_sm = sparse.model.matrix(AVG_REMOVAL_RATE_OUTLIER_FLAG ~ . -1, data = trainSmote)
#sparse_matrix_sm = data.matrix(trainSmote[,-1], missing = NaN)
#output_vector_sm=trainSmote[,"AVG_REMOVAL_RATE_OUTLIER_FLAG"] #Not used in classification

# Levels are needed when using factors in caret for classification
ysm <- as.factor(trainSmote[,1])
levels(ysm) <-  c("Normal", "Outlier")



# set up the cross-validated hyper-parameter search
xgb_grid_sm = expand.grid(
  nrounds = c(1000),
  eta = c(0.01, 0.001,0.0001),
  max_depth = c(3,4,5,6,7,8,9),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 1
)

# pack the training control parameters
xgb_trcontrol_sm = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  #twoClassSummary allows ROC to be used as performance metric, default is accuracy
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  savePredictions = TRUE
)


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
set.seed(123)
xgb_train_sm = train(
  x = sparse_matrix_sm,
  y = ysm,
  trControl = xgb_trcontrol_sm,
  tuneGrid = xgb_grid_sm,
  method = "xgbTree",
  objective = "binary:logistic"
)

xgb_train_sm
plot(xgb_train_sm)



# Run model on test data

sparse_matrix_test_sm = sparse.model.matrix(AVG_REMOVAL_RATE_OUTLIER_FLAG ~ .-AVG_REMOVAL_RATE -WAFER_ID -STAGE -1, data = test)
xgb_pred_sm <- predict(xgb_train_sm, newdata = sparse_matrix_test_sm)


# Create levels in test data so a confusion matrix can be constructed
levels(test$AVG_REMOVAL_RATE_OUTLIER_FLAG) <-  c("No", "Yes")
table(xgb_pred_sm,test$AVG_REMOVAL_RATE_OUTLIER_FLAG)

# Use validation function for accuracy, precision, etc

validate(test$AVG_REMOVAL_RATE_OUTLIER_FLAG,xgb_pred_sm)
