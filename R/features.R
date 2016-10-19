


# wafers in multiple stages -----------------------------------------------

multiple_stage_flag <- train_response %>% 
  group_by(WAFER_ID) %>% 
  count() %>% 
  mutate(MULTIPLE_STAGE_FLAG = if_else(n > 1, 1, 0)) %>%
  select(-n)


# rework flag -------------------------------------------------------------

# give each stage and chamber an ordered ID
sequence_id <- tibble(
  STAGE_CHAMBER = c('A_1', 'A_2', 'A_3', 'A_4', 'A_5', 'A_6', 'B_4', 'B_5', 'B_6')
  ) %>% mutate(SEQUENCE_ID = row_number())

# example of rework
ggplot(train_df %>% filter(WAFER_ID == -4019511766),
       aes(x = TIMESTAMP, y = CHAMBER, color = STAGE)) +
  geom_point() +
  geom_vline(xintercept = 483966563)

# get timestamp when sequence goes down
rework_timestamps <- train_df %>% select(WAFER_ID, TIMESTAMP, STAGE, CHAMBER) %>%
  unite(STAGE_CHAMBER, STAGE, CHAMBER) %>%
  inner_join(sequence_id, by = "STAGE_CHAMBER") %>%
  group_by(WAFER_ID) %>%
  #filter(WAFER_ID == -4019511766) %>%
  arrange(TIMESTAMP) %>%
  mutate(REWORK_FLAG = if_else(SEQUENCE_ID < lag(SEQUENCE_ID, 1), 1, 0)) %>%
  filter(REWORK_FLAG == 1) %>%
  ungroup() %>%
  mutate(REWORK_TIMESTAMP = TIMESTAMP) %>%
  select(WAFER_ID, REWORK_TIMESTAMP) # may need to check for multiple reworks
  
# overall_duration --------------------------------------------------------

# compute the overall time duration from start to finish
processing_durations <- train_df %>% select(WAFER_ID, TIMESTAMP) %>%

  group_by(WAFER_ID) %>%
  mutate(MIN_TIMESTAMP = min(TIMESTAMP),
         MAX_TIMESTAMP = max(TIMESTAMP)
  ) %>%
  ungroup() %>%
  select(-TIMESTAMP) %>%
  distinct() %>%
  mutate(TOTAL_PROCESSING_DURATION = MAX_TIMESTAMP - MIN_TIMESTAMP) %>%
  select(-MIN_TIMESTAMP, -MAX_TIMESTAMP)

glimpse(processing_durations)

# stage and chamber durations ---------------------------------------------

# compute the duration within each chamber and account for rework
stage_chamber_durations <- train_df %>% 
  #filter(WAFER_ID == -4019511766) %>% 
  select(WAFER_ID, TIMESTAMP, STAGE, CHAMBER) %>% 
  left_join(rework_timestamps, by = "WAFER_ID") %>%
  replace_na(list(REWORK_TIMESTAMP = 9999999999)) %>%
  mutate(REWORK_PASS = if_else(TIMESTAMP < REWORK_TIMESTAMP, 0L, 1L)) %>%
  select(-REWORK_TIMESTAMP) %>%
  # polishing duration
  group_by(WAFER_ID, STAGE, CHAMBER, REWORK_PASS) %>%
  mutate(STEP_DURATION = TIMESTAMP - lag(TIMESTAMP, 1L)) %>%
  ungroup() %>%
  group_by(WAFER_ID, STAGE, CHAMBER) %>%
  summarize(DURATION = sum(STEP_DURATION, na.rm = TRUE)) %>%
  unite(STAGE_CHAMBER, STAGE, CHAMBER) %>%
  #unite(STAGE_CHAMBER_PASS, STAGE_CHAMBER, REWORK_PASS) %>%
  spread(STAGE_CHAMBER, DURATION, fill = 0) %>%
  mutate(SEQUENCE = paste0(
    "S",
    if_else(A_1 > 0 , 1, 0), 
    if_else(A_2 > 0 , 1, 0), 
    if_else(A_3 > 0 , 1, 0), 
    if_else(A_4 > 0 , 1, 0),  
    if_else(A_5 > 0 , 1, 0), 
    if_else(A_6 > 0 , 1, 0),  
    if_else(B_4 > 0 , 1, 0),  
    if_else(B_5 > 0 , 1, 0),  
    if_else(B_6 > 0 , 1, 0)
  )) %>%
  ungroup() %>%
  group_by(WAFER_ID) %>%
  mutate(
    TOTAL_POLISHING_DURATION = sum(A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + B_4 + B_5 + B_6)
  ) %>% 
  ungroup()

glimpse(stage_chamber_durations)


# combined durations ------------------------------------------------------

# combine the overall processing duration and chamber duration
combined_durations <- processing_durations %>% 
  inner_join(stage_chamber_durations, by = "WAFER_ID") %>%
  mutate(PRCT_TIME_POLISHING = TOTAL_POLISHING_DURATION / TOTAL_PROCESSING_DURATION)

glimpse(combined_durations)

# simple aggregates -------------------------------------------------------

# group by STAGE and CHAMBER
agrgts <- train_df %>% 
  select(-MACHINE_ID, -MACHINE_DATA, -AVG_REMOVAL_RATE) %>% 
  group_by(WAFER_ID, STAGE, CHAMBER) %>% 
  summarize_each(funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm = TRUE))) %>% 
  ungroup() %>%
  gather(var, val, -WAFER_ID, -STAGE, -CHAMBER) %>%
  unite(CHAMBER_VAR, var, CHAMBER) %>%
  spread(CHAMBER_VAR, val, fill = 0)

glimpse(agrgts)

# group by STAGE
agrgts_2 <- train_df %>% 
  select(-MACHINE_ID, -MACHINE_DATA, -AVG_REMOVAL_RATE) %>% 
  group_by(WAFER_ID, STAGE) %>% 
  summarize_each(funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm = TRUE))) %>% 
  ungroup() %>%
  gather(var, val, -WAFER_ID, -STAGE) %>%
  spread(var, val, fill = 0) %>%
  mutate(STAGE_DURATION = TIMESTAMP_max - TIMESTAMP_min)

glimpse(agrgts_2)


# all features ------------------------------------------------------------

features <- train_response %>%
  inner_join(combined_durations, by = "WAFER_ID") %>%
  inner_join(agrgts_2, by = c("WAFER_ID", "STAGE"))

glimpse(features)
