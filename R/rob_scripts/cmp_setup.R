library(sqldf)
library(glmnet)
library(lattice)
library(xgboost)
library(Matrix)
library(randomForest)
library(tidyverse)
library(caret)
library(pROC)



library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
# machine learning code goes in here
stopCluster(cl)
