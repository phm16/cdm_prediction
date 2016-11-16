train <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/train_df2.csv")
test <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/test_df.csv")

str(train) == str(test)
x <- train[,-c(1,2,3,4,16)]
y <- as.factor(train[,4])
levels(y) <-  c("No", "Yes")

?levels

# set up the cross-validated hyper-parameter search
rf_grid_ol = expand.grid(
   mtry = c(27)
)

# pack the training control parameters
rf_trcontrol_ol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  savePredictions = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
set.seed(123)
rf_train_ol = train(x = x,
                    y = y,
  #data = train,
  trControl = rf_trcontrol_ol,
  tuneGrid = rf_grid_ol,
  method = 'rf',
  ntrees = 1000
)

rf_train_ol
varImp(rf_train_ol)


# Test model on new data ------------------------------------------

z <- test[,-c(1,2,3,4,16)]

# Duplicate labels from training set into test set so data is identical
#levels(z$SEQUENCE) <- levels(x$SEQUENCE)
rf_preds <- predict(rf_train_ol, newdata = z, ntrees = 1000)



# Create levels in test data so a confusion matrix can be constructed
levels(test$AVG_REMOVAL_RATE_OUTLIER_FLAG) <-  c("No", "Yes")
table(rf_preds,test$AVG_REMOVAL_RATE_OUTLIER_FLAG)

validate(test$AVG_REMOVAL_RATE_OUTLIER_FLAG,rf_preds)


# Roc Curve is a work in progress-----------------------------------


# Max Kuhn's example This one works!:
rf.probs <- predict(rf_train_ol, z, ntrees = 1000, type = "prob")
rf.probs

rpartROC <- roc(test$AVG_REMOVAL_RATE_OUTLIER_FLAG, rf.probs[, "No"])
plot(rpartROC, type = "S", main = "Random Forest ROC Curve - AUC 0.9773")

