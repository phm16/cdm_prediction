

# split location
split_location <- floor(nrow(mdl_df) * 1.0)

# get the minimum timestamp for each observation

train_df <- mdl_df %>%
  arrange(TIMESTAMP_min) %>%
  mutate(INDEX = row_number()) %>%
  filter(INDEX <= split_location) %>%
  select(-INDEX)

# write train file
# write_csv(train_df, "data/train_df.csv")

test_df <- mdl_df %>% anti_join(train_df, by = c("WAFER_ID", "STAGE"))

# write test file
# write_csv(test_df, "data/test_df.csv")