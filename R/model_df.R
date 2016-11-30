

# create model datasets ---------------------------------------------------

train_df <- features_function(train_df_ts)
validation_df <- features_function(validation_df_ts)
test_df <- features_function(test_df_ts)

# write files -------------------------------------------------------------

# write_csv(train_df, "data/train_df.csv")
# write_csv(validation_df, "data/validation_df.csv")
# write_csv(test_df, "data/test_df.csv")

# write_rds(train_df, "data/train_df.rds")
# write_rds(validation_df, "data/validation_df.rds")
# write_rds(test_df, "data/test_df.rds")

# remove or add features --------------------------------------------------

mdl_df <- train_df %>%
  select(
    -WAFER_ID,
    -MULTIPLE_STAGE_FLAG,
    -STAGE,
    -REPOLISH_FLAG,
    -REQUIRED_REPOLISH_FLAG,
    -POLISH_TYPE,
    -SEQUENCE,
    -TOTAL_POLISHING_DURATION,
    -TOTAL_PROCESSING_DURATION,
    -PRCT_TIME_POLISHING,
    -STAGE_DURATION,
    -LONG_POLISH_FLAG,
    -START_TO_START_LAG,
    -FIRST_CPP_FLAG,
    -CPP_INC,
    -OVERALL_CPP,
    -JOB_WITHIN_CPP,
    #-ends_with("_min_P123"),
    #-ends_with("_max_P123"),
    #-starts_with("USAGE_OF_MEMBRANE"),
    #-starts_with("USAGE_OF_PRESSURIZED_SHEET"),
    -ends_with("min"),
    -ends_with("max"),
    -ends_with("sum"),
    -ends_with("mean"),
    -ends_with("sum_P123"),
    -contains("REPLACED_FLAG"),
    -contains("_sd_"),
    #-starts_with("AVG_REMOVAL_RATE_lag"),
    -starts_with("TIMESTAMP"),
    -ends_with("_total"),
    -USAGE_OF_BACKING_FILM_mean_P123,
    -USAGE_OF_BACKING_FILM_sd_P123,
    -USAGE_OF_BACKING_FILM_sum_P123,
    -USAGE_OF_DRESSER_mean_P123,
    -USAGE_OF_DRESSER_sd_P123,
    -USAGE_OF_DRESSER_sum_P123,
    -USAGE_OF_MEMBRANE_mean_P123,
    -USAGE_OF_MEMBRANE_sd_P123,
    -USAGE_OF_MEMBRANE_sum_P123,
    -USAGE_OF_POLISHING_TABLE_mean_P123,
    -USAGE_OF_POLISHING_TABLE_sd_P123,
    -USAGE_OF_POLISHING_TABLE_sum_P123,
    -USAGE_OF_PRESSURIZED_SHEET_mean_P123,
    -USAGE_OF_PRESSURIZED_SHEET_sd_P123,
    -USAGE_OF_PRESSURIZED_SHEET_sum_P123,
    -USAGE_OF_DRESSER_TABLE_mean_P123,
    -USAGE_OF_DRESSER_TABLE_sd_P123,
    -USAGE_OF_DRESSER_TABLE_sum_P123
    
  )

glimpse(mdl_df)
  