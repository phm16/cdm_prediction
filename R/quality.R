

# function to calculate yield based on assumed error threshold
yield <- function(predicted, actual, error_threshold = 20) {
  
  n_validation <- length(actual)
  
  pred_df <- data.frame(
    model = predicted, 
    actual = actual
  ) %>%
    mutate(
      abs_error = abs(actual - model),
      good = if_else(abs_error < error_threshold, 1L, 0L)
      
    )
  
  sum(pred_df$good) / n_validation
  
}


# predict one yield
yield(glm_null_pred, validation_df$AVG_REMOVAL_RATE, error_threshold = 10)
yield(glm_mdl_1_pred, validation_df$AVG_REMOVAL_RATE, error_threshold = 10)
yield(rf_pred_1, validation_df$AVG_REMOVAL_RATE, error_threshold = 10)
yield(knn_station_pred$pred, validation_responses, error_threshold = 10)

model_preds <- data.frame(
  null = glm_null_pred,
  glm = glm_mdl_1_pred,
  rf = rf_pred_1
)

# write_rds(model_preds, "data/model_preds.rds")
# write_rds(knn_station_pred, "data/knn_station_pred.rds")

errors <- seq(1, 50, 1)

qplot(
  x = errors,
  y = sapply(errors, function(errors) yield(glm_null_pred, validation_df$AVG_REMOVAL_RATE, errors)),
  geom = "line",
  xlab = "absolute error",
  ylab = "yield"
)

qplot(
  x = errors,
  y = sapply(errors, function(errors) yield(glm_mdl_1_pred, validation_df$AVG_REMOVAL_RATE, errors)),
  geom = "line",
  xlab = "absolute error",
  ylab = "yield"
)


yield_curves <- data.frame(
  absolute_error = errors,
  no_predictor_model = sapply(errors, function(errors) yield(model_preds$null, validation_df$AVG_REMOVAL_RATE, errors)),
  linear_model = sapply(errors, function(errors) yield(model_preds$glm, validation_df$AVG_REMOVAL_RATE, errors)),
  random_forest_model = sapply(errors, function(errors) yield(model_preds$rf, validation_df$AVG_REMOVAL_RATE, errors)),
  knn_model = sapply(errors, function(errors) yield(knn_station_pred$pred, validation_responses, errors))
) %>%
  gather(model, yield, -absolute_error)

yield_curves %>% filter(absolute_error == 10)

ggplot(
  yield_curves,
  aes(x = absolute_error,
      y = yield,
      color = model
  )) + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_label(data = yield_curves %>% filter(absolute_error == 10),
             aes(x = absolute_error, 
                 label = paste0(round(yield, 3) * 100, " %"), color = model)) +
  labs(title = "Yield vs. Acceptable Error", x = "Absolute Error", y = "Yield", color = "Model")



