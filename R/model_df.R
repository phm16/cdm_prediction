

# remove or add features --------------------------------------------------

mdl_df <- features %>%
  select(-ends_with("min"),
         -ends_with("max"),
         -ends_with("sum"),
         -AVG_REMOVAL_RATE_OUTLIER_FLAG,
         TIMESTAMP_min)

glimpse(mdl_df)
  