

train <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP DATA SET/2016 PHM DATA CHALLENGE CMP DATA SET/data/train_df.csv")
validation <- read.csv("F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/2016 PHM DATA CHALLENGE CMP VALIDATION DATA SET/2016 PHM DATA CHALLENGE CMP VALIDATION DATA SET/validation_df.csv")

train_df <- train %>% filter(AVG_REMOVAL_RATE < 1000)



x = model.matrix(AVG_REMOVAL_RATE ~ . -WAFER_ID -SEQUENCE - 1, data = train_df)
y = train_df$AVG_REMOVAL_RATE
x2 <- model.matrix(AVG_REMOVAL_RATE ~ . -WAFER_ID -SEQUENCE -1, data = validation)


### Original lambda.seq do not atler


lambda.seq <-seq(2, .000001, by = -0.001)


# more managable lambda seq, .01 turns out to be best value
lambda.seq <- c(.0001,.001,.009,.01,.011, .05,1,1.01)


# Using train control seems to create sub optimal results

lasso_trcontrol_ol = trainControl(
  method = "cv",
  number = 5
#  verboseIter = TRUE,
#  returnData = FALSE,
#  returnResamp = "all",                                                        # save losses across all models
#  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
#  summaryFunction = twoClassSummary,
#  allowParallel = TRUE
#  savePredictions = TRUE
)

set.seed(123)
cdm_lasso <- train(x, y, 
              method ="glmnet", 
              family = "gaussian",
              tuneGrid = expand.grid(alpha = 1, lambda = lambda.seq)
              #trControl = lasso_trcontrol_ol
              #preProcess =  c("center","scale")
              )

lasso_vars <- coef(cdm_lasso$finalModel, s= .009)

cdm_lasso

lvars <- as.matrix(lasso_vars)

lvarsdm <- as.data.frame(lvars)

write.csv(lvarsdm, file = "F:/Storage/Google Drive/Northwestern/Predict 498 MSPA Capstone/lasso_vars.csv")

plot(cdm_lasso, xvar = 'lambda', main = 'Lasso - RMSE vs Lambda')


set.seed(123)
fit <- glmnet(x,y, alpha = 1, lambda = .009)
coef(fit)
plot(fit)
fit

fit$finalModel


lasso_preds <- predict(cdm_lasso, x2)
lasso_rmse <- sqrt(mean((lasso_preds - validation$AVG_REMOVAL_RATE)^2))
lasso_rmse


fit_preds <- predict(fit, x2)
fit_rmse <- sqrt(mean((fit_preds - validation$AVG_REMOVAL_RATE)^2))
fit_rmse

###### Using GLMNET Cross Validation


cdm_lasso2 <- cv.glmnet(x, y, family="gaussian", lambda = lambda.seq)

?cv.glmnet

cdm_lasso2

plot(cdm_lasso2, xvar='lambda')

cdm_lasso2$lambda.min

cdm_lasso2$lambda.1se

lasso_preds2 <- predict(cdm_lasso2, x2)
lasso_rmse2 <- sqrt(mean((lasso_preds2 - validation$AVG_REMOVAL_RATE)^2))
lasso_rmse2

#### To get glmnet coefs, a specific lambda is need, a grid CANNOT be used, this uses cv to select the best lambda

fit2 = glmnet(x, y, alpha = 1, lambda=cv.glmnet(x, y, lambda = lambda.seq)$lambda.min)

fit2

plot(fit2)
coef(fit2)

#### Doesn't perform as well but more interperatable
fit_preds2 <- predict(fit2, x2)
fit_rmse2 <- sqrt(mean((fit_preds2 - validation$AVG_REMOVAL_RATE)^2))
fit_rmse2




