library(gbm)

formula <- AVG_REMOVAL_RATE_OUTLIER_FLAG ~ .

?gbm

gbmSmote <- gbm(formula,
                data = trainSmote,
                shrinkage = .01,
                distribution = "bernoulli",
                n.trees =  1000,
                interaction.depth = 7)

gbmSpred <- predict(gbmSmote, newdata = test, n.trees = 1000)


SmotePreds <- as.data.frame(gbmSpred)
