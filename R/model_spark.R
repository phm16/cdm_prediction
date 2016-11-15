library(sparklyr)
library(dplyr)

# connect to a local spark cluster
sc <- spark_connect(master = "local")

# read option 1 -----------------------------------------------------------

# read in training data
train_df <- read_csv("data/train_df.csv") %>% select_if(is.numeric)

# copy R data frame to spark cluster
train_df_spark <- copy_to(
  dest = sc, 
  df = train_df, 
  name = "train_df_spark",
  overwrite = TRUE, 
  memory = FALSE
)


# read option 2 -----------------------------------------------------------

# copy csv directly to spark cluster
train_df_spark <- spark_read_csv(
  sc, 
  name = "train_df_spark",
  path = "data/train_df.csv", 
  overwrite = TRUE, 
  memory = FALSE
)

# split the data into train and test sets
partitions <- train_df_spark %>%
  sdf_partition(training = 0.7, test = 0.3)

partitions <- tbl(sc, "train_df_spark") %>%
  sdf_partition(training = 0.7, test = 0.3)

# model performance -------------------------------------------------------

# function to calculate mean squared error
estimate_mse <- function(df, model) {
  
  sdf_predict(model, df) %>%
    mutate(resid = AVG_REMOVAL_RATE - prediction) %>%
    summarize(MSE = mean(resid ^ 2)) %>%
    collect()
  
}

# MLlib decision tree -----------------------------------------------------

# train a single decision tree
spark_tree <- partitions$training %>%
  ml_decision_tree(
    response = "AVG_REMOVAL_RATE",
    features = c("STATION", "CENTER_AIR_BAG_PRESSURE_sum_P123"),
    type = "regression"
  )

# model performance
sapply(partitions, estimate_mse, model = spark_tree)

map2(c(spark_tree, spark_tree), partitions, estimate_mse)

# tree feature importance
ml_tree_feature_importance(sc, spark_tree)

# random forest -----------------------------------------------------------

# train a random forest
spark_rf <- partitions$training %>%
  ml_random_forest(
    response = "AVG_REMOVAL_RATE",
    features = c(
      "CENTER_AIR_BAG_PRESSURE_sum_P123", 
      "USAGE_OF_DRESSER_max",
      "SLURRY_FLOW_LINE_A_mean_P123",
      "SLURRY_FLOW_LINE_B_mean_P123",
      "SLURRY_FLOW_LINE_C_mean_P123",
      "STAGE_ROTATION_mean_P123",
      "HEAD_ROTATION_mean_P123"
      ),
    type = "regression",
    num.trees = 1000
  )

# train a random forest
spark_rf <- partitions$training %>%
  ml_random_forest(AVG_REMOVAL_RATE ~ . -WAFER_ID,
    num.trees = 1000
  ) 
                   
# model performance
sapply(partitions, estimate_mse, model = spark_rf)                 

# feature importance
ml_tree_feature_importance(sc, spark_rf)


# boosted trees -----------------------------------------------------------

# train boosted trees
spark_boosted_trees <- partitions$training %>%
  ml_gradient_boosted_trees(
    response = "AVG_REMOVAL_RATE",
    features = c(
      "STATION", 
      "CENTER_AIR_BAG_PRESSURE_sum_P123", 
      "USAGE_OF_DRESSER_max",
      "SLURRY_FLOW_LINE_A_mean_P123",
      "SLURRY_FLOW_LINE_B_mean_P123",
      "SLURRY_FLOW_LINE_C_mean_P123",
      "STAGE_ROTATION_mean_P123",
      "HEAD_ROTATION_mean_P123",
      "POLISH_TYPE"
    ),
    type = "regression"
  )   

# model performance
sapply(partitions, estimate_mse, model = spark_boosted_trees)  

# feature importance
ml_tree_feature_importance(sc, spark_boosted_trees)


# standard random forest --------------------------------------------------

rf <- randomForest(
  AVG_REMOVAL_RATE ~ 
    CENTER_AIR_BAG_PRESSURE_sum_P123 + 
    USAGE_OF_DRESSER_max +
    SLURRY_FLOW_LINE_A_mean_P123 +
    SLURRY_FLOW_LINE_B_mean_P123 +
    SLURRY_FLOW_LINE_C_mean_P123 +
    STAGE_ROTATION_mean_P123 +
    HEAD_ROTATION_mean_P123,
  data = train_df %>% sample_frac(0.7),
  ntree = 500
)

rf <- randomForest(
  AVG_REMOVAL_RATE ~ . -WAFER_ID,
  data = train_df %>% sample_frac(0.7),
  ntree = 100
)



