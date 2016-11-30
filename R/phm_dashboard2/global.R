library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(caret)
library(purrr)
library(tidyr)

train_df <- readRDS('data/train_df.rds')

train_df_ts_last_10_jobs <- readRDS('data/train_df_ts_last_10_jobs.rds')

wafer_ids <- train_df_ts_last_10_jobs %>% distinct(WAFER_ID) %>% .$WAFER_ID

validation_df <- readRDS('data/validation_df.rds')

model_preds <- readRDS('data/model_preds.rds')

knn_station_pred <- readRDS('data/knn_station_pred.rds')

stations <- c("A123", "A456", "B456")

errors <- seq(1, 40, 1)

# split the validation set by station
validation_responses <- map(stations, function(station) {
  
  validation_df %>% 
    filter(STATION == station) %>%
    select(AVG_REMOVAL_RATE) %>%
    .$AVG_REMOVAL_RATE
  
}) %>% flatten_dbl(.)


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

yield_curves <- data.frame(
  absolute_error = errors,
  no_predictor_model = sapply(errors, function(errors) yield(model_preds$null, validation_df$AVG_REMOVAL_RATE, errors)),
  linear_model = sapply(errors, function(errors) yield(model_preds$glm, validation_df$AVG_REMOVAL_RATE, errors)),
  random_forest_model = sapply(errors, function(errors) yield(model_preds$rf, validation_df$AVG_REMOVAL_RATE, errors)),
  knn_model = sapply(errors, function(errors) yield(knn_station_pred$pred, validation_responses, errors))
) %>%
  gather(model, yield, -absolute_error)


ctrl <-
  trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    savePredictions = "final"
  )

set.seed(59)
glm_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = train_df %>% filter(!STATION == "A123") %>%
    select(
      AVG_REMOVAL_RATE,
      AVG_REMOVAL_RATE_lag1,
      STATION,
      CENTER_AIR_BAG_PRESSURE_mean_P123,
      PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
      SLURRY_FLOW_LINE_A_mean_P123,
      SLURRY_FLOW_LINE_C_mean_P123,
      USAGE_OF_DRESSER_max_P123,
      USAGE_OF_DRESSER_TABLE_max_P123,
      HEAD_ROTATION_mean_P123,
      WAFER_ROTATION_mean_P123
    ),
  method = "glm",
  trControl = ctrl
)

timestamp_list <- train_df %>% distinct(TIMESTAMP_min)

wafer_id_list <- train_df %>% distinct(WAFER_ID) %>% arrange(WAFER_ID) %>% .$WAFER_ID

var_list <- names(train_df %>% select(-WAFER_ID))

station_list <- c("A456", "B456")

head_rotation_list <- c(160L, 192L)

wafer_rotation_list <- c(23L, 35L)

last_vals <- train_df %>%
  filter(TIMESTAMP_min == max(train_df$TIMESTAMP_min))

dresser_life <- (last_vals$USAGE_OF_DRESSER_max_P123 / 700)

backing_film_life <- (last_vals$USAGE_OF_BACKING_FILM_max_P123 / 10000)