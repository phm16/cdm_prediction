

# station -----------------------------------------------------------------

station <- base_df %>%
  distinct(WAFER_ID, STAGE, POLISH_TYPE, STATION)

glimpse(station)

# wafers in multiple stages -----------------------------------------------

# if wafer count is greater than 1, then the wafer is processed in multiple chambers
multiple_stage_flag <- response %>% 
  group_by(WAFER_ID) %>% 
  count() %>% 
  mutate(MULTIPLE_STAGE_FLAG = if_else(n > 1, 1L, 0L)) %>%
  select(-n)

glimpse(multiple_stage_flag)

# component replacement flags ---------------------------------------------

# flag when there is a large drop in usage (signaling a replacement)
replace_flags <- base_df %>% select(WAFER_ID, STAGE, STATION, TIMESTAMP, starts_with("USAGE")) %>%
  gather(VAR, VAL, -WAFER_ID, -STAGE, -STATION, -TIMESTAMP) %>%
  group_by(VAR, STATION) %>%
  mutate(
    LAG_VAL = lag(VAL, 1, default = 0L)
  ) %>%
  ungroup() %>%
  mutate(
    VAR = paste0(str_replace(VAR, "USAGE_OF_", ""), "_REPLACED_FLAG")
  )

a123 <- replace_flags %>% 
  filter(STATION == "A123") %>%
  arrange(TIMESTAMP) %>%
  mutate(
    REPLACE_FLAG = case_when(
      .$VAR == "BACKING_FILM_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "DRESSER_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 100)) ~ 1L,
      .$VAR == "DRESSER_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "MEMBRANE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 20)) ~ 1L,
      .$VAR == "POLISHING_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 50)) ~ 1L,
      .$VAR == "PRESSURIZED_SHEET_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 500)) ~ 1L,
      TRUE ~ 0L
    ))

a456 <- replace_flags %>% 
  filter(STATION == "A456") %>%
  arrange(TIMESTAMP) %>%
  mutate(
    REPLACE_FLAG = case_when(
      .$VAR == "BACKING_FILM_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "DRESSER_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 100)) ~ 1L,
      .$VAR == "DRESSER_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "MEMBRANE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 20)) ~ 1L,
      .$VAR == "POLISHING_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 50)) ~ 1L,
      .$VAR == "PRESSURIZED_SHEET_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 500)) ~ 1L,
      TRUE ~ 0L
    ))

b456 <- replace_flags %>% 
  filter(STATION == "B456") %>%
  arrange(TIMESTAMP) %>%
  mutate(
    REPLACE_FLAG = case_when(
      .$VAR == "BACKING_FILM_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "DRESSER_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 100)) ~ 1L,
      .$VAR == "DRESSER_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 1000)) ~ 1L,
      .$VAR == "MEMBRANE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 20)) ~ 1L,
      .$VAR == "POLISHING_TABLE_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 50)) ~ 1L,
      .$VAR == "PRESSURIZED_SHEET_REPLACED_FLAG" & (.$LAG_VAL > (.$VAL + 500)) ~ 1L,
      TRUE ~ 0L
    ))

replace_flags <- a123 %>% bind_rows(a456) %>% bind_rows(b456)

rm(list = c("a123", "a456", "b456"))

# spread the replacement flags to columns
replace_flags_spread <- replace_flags %>%
  filter(REPLACE_FLAG == 1) %>%
  select(
    WAFER_ID,
    STAGE,
    TIMESTAMP,
    VAR,
    REPLACE_FLAG
  ) %>%
  distinct() %>%
  spread(VAR, REPLACE_FLAG, fill = 0L) %>%
  distinct()

glimpse(replace_flags_spread)

# rework flag -------------------------------------------------------------

# give each stage and chamber an ordered ID
sequence_id <- tibble(
  STAGE_CHAMBER = c('A_1', 'A_2', 'A_3', 'A_4', 'A_5', 'A_6', 'B_4', 'B_5', 'B_6')
  ) %>% mutate(SEQUENCE_ID = row_number())

# get timestamp when a wafer goes back to a previous chamber
rework_timestamps <- base_df %>% select(WAFER_ID, TIMESTAMP, STAGE, CHAMBER) %>%
  unite(STAGE_CHAMBER, STAGE, CHAMBER) %>%
  inner_join(sequence_id, by = "STAGE_CHAMBER") %>%
  group_by(WAFER_ID) %>%
  arrange(TIMESTAMP) %>%
  mutate(REWORK_FLAG = if_else(SEQUENCE_ID < lag(SEQUENCE_ID, 1), 1, 0)) %>%
  filter(REWORK_FLAG == 1) %>%
  ungroup() %>%
  mutate(REWORK_TIMESTAMP = TIMESTAMP) %>%
  select(WAFER_ID, REWORK_TIMESTAMP) # may need to check for multiple reworks
  
# overall_duration --------------------------------------------------------

# compute the overall time duration from start to finish
processing_durations <- base_df %>% 
  select(WAFER_ID, TIMESTAMP) %>%
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
stage_chamber_durations <- base_df %>% 
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

# group by stage and chamber
agrgts_stage_chamber <- base_df %>% 
  #filter(WAFER_ID == 2062207654) %>% 
  group_by(WAFER_ID, TIMESTAMP, STAGE, POLISH_TYPE, STATION, POLISH_PHASE, CHAMBER) %>%
  summarize_each(funs(mean(., na.rm = TRUE))) %>% # account for timestamps with multiple values
  #select(TIMESTAMP, CHAMBER, WAFER_ID, STAGE, STATION, USAGE_OF_BACKING_FILM) %>%
  ungroup() %>%
  select(-MACHINE_ID, -MACHINE_DATA, -AVG_REMOVAL_RATE, -POLISH_TYPE, -POLISH_PHASE) %>%
  distinct() %>%
  left_join(replace_flags_spread, by = c("WAFER_ID", "STAGE", "TIMESTAMP")) %>%
  replace_na(
    list(
      "BACKING_FILM_REPLACED_FLAG" = 0L,
      "DRESSER_REPLACED_FLAG" = 0L,
      "MEMBRANE_REPLACED_FLAG" = 0L,
      "POLISHING_TABLE_REPLACED_FLAG" = 0L,
      "PRESSURIZED_SHEET_REPLACED_FLAG" = 0L
    )
  ) %>%
  distinct() %>%
  group_by(WAFER_ID, STAGE, STATION, CHAMBER) %>% 
  summarize_each(funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm = TRUE), sum(., na.rm = TRUE))) %>% 
  ungroup() %>%
  gather(var, val, -WAFER_ID, -STAGE, -STATION, -CHAMBER) %>%
  unite(CHAMBER_VAR, var, CHAMBER) %>%
  spread(CHAMBER_VAR, val, fill = 0) %>%
  select(-STATION)

glimpse(agrgts_stage_chamber)

# group by STAGE
agrgts_stage <- base_df %>% 
  #filter(WAFER_ID == 2062207654) %>% 
  group_by(WAFER_ID, TIMESTAMP, STAGE, POLISH_TYPE, STATION, POLISH_PHASE, CHAMBER) %>%
  summarize_each(funs(mean(., na.rm = TRUE))) %>% # account for timestamps with multiple values
  #select(TIMESTAMP, CHAMBER, WAFER_ID, STAGE, STATION, USAGE_OF_BACKING_FILM) %>%
  ungroup() %>%
  #select(TIMESTAMP, WAFER_ID, STAGE, STATION, USAGE_OF_BACKING_FILM) %>%
  select(-MACHINE_ID, -MACHINE_DATA, -STATION, -POLISH_TYPE, -POLISH_PHASE, -CHAMBER, -AVG_REMOVAL_RATE) %>%
  distinct() %>%
  left_join(replace_flags_spread, by = c("WAFER_ID", "STAGE", "TIMESTAMP")) %>%
  replace_na(
    list(
      "BACKING_FILM_REPLACED_FLAG" = 0L,
      "DRESSER_REPLACED_FLAG" = 0L,
      "MEMBRANE_REPLACED_FLAG" = 0L,
      "POLISHING_TABLE_REPLACED_FLAG" = 0L,
      "PRESSURIZED_SHEET_REPLACED_FLAG" = 0L
    )
  ) %>%
  group_by(WAFER_ID, STAGE) %>% 
  summarize_each(funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm = TRUE), sum(., na.rm = TRUE))) %>% 
  ungroup() %>%
  gather(var, val, -WAFER_ID, -STAGE) %>%
  spread(var, val, fill = 0) %>%
  mutate(STAGE_DURATION = TIMESTAMP_max - TIMESTAMP_min)

glimpse(agrgts_stage)

# group by polishing phase
agrgts_polishing_phase <- base_df %>% 
  filter(POLISH_PHASE == "P123") %>%
  #filter(WAFER_ID == 2062207654) %>% 
  group_by(WAFER_ID, TIMESTAMP, STAGE, POLISH_TYPE, STATION, POLISH_PHASE, CHAMBER) %>%
  summarize_each(funs(mean(., na.rm = TRUE))) %>% # account for timestamps with multiple values
  #select(TIMESTAMP, CHAMBER, WAFER_ID, STAGE, STATION, USAGE_OF_BACKING_FILM) %>%
  ungroup() %>%
  select(-MACHINE_ID, -MACHINE_DATA, -AVG_REMOVAL_RATE, -STATION, -POLISH_TYPE, -CHAMBER) %>%
  distinct() %>%
  left_join(replace_flags_spread, by = c("WAFER_ID", "STAGE", "TIMESTAMP")) %>%
  replace_na(
    list(
      "BACKING_FILM_REPLACED_FLAG" = 0L,
      "DRESSER_REPLACED_FLAG" = 0L,
      "MEMBRANE_REPLACED_FLAG" = 0L,
      "POLISHING_TABLE_REPLACED_FLAG" = 0L,
      "PRESSURIZED_SHEET_REPLACED_FLAG" = 0L
    )
  ) %>%
  distinct() %>%
  group_by(WAFER_ID, STAGE, POLISH_PHASE) %>% 
  summarize_each(funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm = TRUE), sum(., na.rm = TRUE))) %>% 
  ungroup() %>%
  gather(var, val, -WAFER_ID, -STAGE, -POLISH_PHASE) %>%
  unite(POLISH_PHASE_VAR, var, POLISH_PHASE) %>%
  spread(POLISH_PHASE_VAR, val, fill = 0)

glimpse(agrgts_polishing_phase)

# all features ------------------------------------------------------------

features1 <- response %>%
  #mutate(AVG_REMOVAL_RATE_OUTLIER_FLAG = if_else(AVG_REMOVAL_RATE > 2000, 1L, 0L)) %>%
  inner_join(station, by = c("WAFER_ID", "STAGE")) %>%
  inner_join(multiple_stage_flag, by = "WAFER_ID") %>%
  inner_join(combined_durations, by = "WAFER_ID") %>%
  inner_join(agrgts_stage, by = c("WAFER_ID", "STAGE")) %>%
  inner_join(agrgts_polishing_phase, by = c("WAFER_ID", "STAGE"))
  #inner_join(agrgts_stage_chamber, by = c("WAFER_ID", "STAGE"))

glimpse(features1)

# start-to-start lag ------------------------------------------------------

start_to_start_lag <- features1 %>%
  select(WAFER_ID, STAGE, STATION, TIMESTAMP_min, AVG_REMOVAL_RATE) %>%
  group_by(STATION) %>%
  arrange(TIMESTAMP_min) %>%
  mutate(
    TIMESTAMP_min_lag = lag(TIMESTAMP_min, 1, default = NA),
    START_TO_START_LAG = TIMESTAMP_min - TIMESTAMP_min_lag
    ) %>%
  ungroup() %>%
  arrange(STATION, TIMESTAMP_min) %>%
  replace_na(list(START_TO_START_LAG = 0))

# ggplot(start_to_start_lag, aes(x = START_TO_START_LAG)) + geom_histogram()

glimpse(start_to_start_lag)

# continuous polishing processes ------------------------------------------

cpp <-
  # overall cpp ---------
  start_to_start_lag %>%
    arrange(TIMESTAMP_min) %>%
    group_by(STATION) %>%
    mutate(FIRST_CPP_FLAG = row_number()) %>%
    mutate(FIRST_CPP_FLAG = if_else(FIRST_CPP_FLAG == 1L, 1L, 0L)) %>%
    ungroup() %>%
    #group_by(STATION) %>%
    mutate(
      CPP_INC = if_else(START_TO_START_LAG > 500 |
                          FIRST_CPP_FLAG == 1, 1L, 0L),
      OVERALL_CPP = cumsum(CPP_INC)
    ) %>%
    # job within cpp ---------
    group_by(OVERALL_CPP) %>%
    arrange(TIMESTAMP_min) %>%
    mutate(
      JOB_WITHIN_CPP = row_number(),
      AVG_REMOVAL_RATE_lag1 = lag(AVG_REMOVAL_RATE, 1L, default = 0L),
      AVG_REMOVAL_RATE_lag2 = lag(AVG_REMOVAL_RATE, 2L, default = 0L),
      AVG_REMOVAL_RATE_lag3 = lag(AVG_REMOVAL_RATE, 3L, default = 0L),
      AVG_REMOVAL_RATE_lag4 = lag(AVG_REMOVAL_RATE, 4L, default = 0L),
      AVG_REMOVAL_RATE_lag5 = lag(AVG_REMOVAL_RATE, 5L, default = 0L)
      ) %>%
    ungroup() %>%
    select(-TIMESTAMP_min, -TIMESTAMP_min_lag, -AVG_REMOVAL_RATE, -STATION)

glimpse(cpp)

# total usage -------------------------------------------------------------

total_usage <- features1 %>%
  select(WAFER_ID, STAGE, starts_with("USAGE")) %>%
  mutate(
    USAGE_OF_BACKING_FILM_total = USAGE_OF_MEMBRANE_max - USAGE_OF_MEMBRANE_min,
    USAGE_OF_DRESSER_total = USAGE_OF_DRESSER_max - USAGE_OF_DRESSER_min,
    USAGE_OF_DRESSER_TABLE_total = USAGE_OF_DRESSER_TABLE_max - USAGE_OF_DRESSER_TABLE_min,
    USAGE_OF_MEMBRANE_total = USAGE_OF_MEMBRANE_max - USAGE_OF_MEMBRANE_min,
    USAGE_OF_POLISHING_TABLE_total = USAGE_OF_POLISHING_TABLE_max - USAGE_OF_POLISHING_TABLE_min,
    USAGE_OF_PRESSURIZED_SHEET_total = USAGE_OF_PRESSURIZED_SHEET_max - USAGE_OF_PRESSURIZED_SHEET_min
  ) %>%
  select(WAFER_ID, STAGE, ends_with("_total"))

glimpse(total_usage)

# impute outliers ---------------------------------------------------------

outliers_imp <- features1 %>%
  filter(AVG_REMOVAL_RATE < 2000) %>%
  select(WAFER_ID, STAGE, STATION, AVG_REMOVAL_RATE) %>%
  group_by(STATION) %>%
  summarize(AVG_REMOVAL_RATE_mean = mean(AVG_REMOVAL_RATE)) %>%
  ungroup()

# avg removal rate lags ---------------------------------------------------

# these are not included in the final feature set - may be useful later for time series

avg_removal_rate_lags <- features1 %>% 
  select(WAFER_ID, STAGE, STATION, TIMESTAMP_min, AVG_REMOVAL_RATE) %>%
  group_by(STATION) %>%
  arrange(TIMESTAMP_min) %>%
  mutate(
    AVG_REMOVAL_RATE_lag1  = lag(AVG_REMOVAL_RATE, 1,  default = 0L),
    AVG_REMOVAL_RATE_lag2  = lag(AVG_REMOVAL_RATE, 2,  default = 0L),
    AVG_REMOVAL_RATE_lag3  = lag(AVG_REMOVAL_RATE, 3,  default = 0L),
    AVG_REMOVAL_RATE_lag4  = lag(AVG_REMOVAL_RATE, 4,  default = 0L),
    AVG_REMOVAL_RATE_lag5  = lag(AVG_REMOVAL_RATE, 5,  default = 0L),
    AVG_REMOVAL_RATE_lag6  = lag(AVG_REMOVAL_RATE, 6,  default = 0L),
    AVG_REMOVAL_RATE_lag7  = lag(AVG_REMOVAL_RATE, 7,  default = 0L),
    AVG_REMOVAL_RATE_lag8  = lag(AVG_REMOVAL_RATE, 8,  default = 0L),
    AVG_REMOVAL_RATE_lag9  = lag(AVG_REMOVAL_RATE, 9,  default = 0L),
    AVG_REMOVAL_RATE_lag10 = lag(AVG_REMOVAL_RATE, 10, default = 0L)
  ) %>%
  ungroup() %>%
  select(-STATION, -TIMESTAMP_min, -AVG_REMOVAL_RATE)

# polish flags ------------------------------------------------------------

polish_flags <- features1 %>%
  select(WAFER_ID, STAGE, STATION, MULTIPLE_STAGE_FLAG, STAGE_DURATION) %>%
  mutate(REPOLISH_FLAG = if_else(STATION == "B456" & MULTIPLE_STAGE_FLAG == 1L, 1L, 0L),
         REQUIRED_REPOLISH_FLAG = if_else(STATION == "A456" & MULTIPLE_STAGE_FLAG == 1L, 1L, 0L),
         LONG_POLISH_FLAG = if_else(STAGE_DURATION >= 350L, 1L, 0L)
  ) %>%
  select(-STATION, -MULTIPLE_STAGE_FLAG, -STAGE_DURATION)

glimpse(polish_flags)

# enhanced features -------------------------------------------------------

features <- features1 %>%
  #inner_join (avg_removal_rate_lags, by = c("WAFER_ID", "STAGE")) %>%
  inner_join(outliers_imp, by = "STATION") %>%
  inner_join(polish_flags, by = c("WAFER_ID", "STAGE")) %>%
  mutate(AVG_REMOVAL_RATE = if_else(
    AVG_REMOVAL_RATE > 2000, 
    AVG_REMOVAL_RATE_mean, 
    AVG_REMOVAL_RATE)
    ) %>%
  inner_join(cpp, by = c("WAFER_ID", "STAGE")) %>%
  inner_join(total_usage, by = c("WAFER_ID", "STAGE")) %>%
  select(-AVG_REMOVAL_RATE_mean)
  
glimpse(features)


