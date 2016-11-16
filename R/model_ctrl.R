

# train control -----------------------------------------------------------

ctrl <-
  trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    savePredictions = "final"
  )