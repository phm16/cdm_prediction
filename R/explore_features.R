


# long data frame ---------------------------------------------------------

train_df_long <- train_df %>% gather(var, val, -WAFER_ID, -STAGE, -STATION, -SEQUENCE)

ggplot(train_df, aes(x = USAGE_OF_BACKING_FILM_max)) + geom_histogram()

train_df %>% count(USAGE_OF_BACKING_FILM_max)

ggplot(train_df %>% mutate(RN = row_number()), 
       aes(x = RN, y = USAGE_OF_BACKING_FILM_max)) + geom_line()

train_df %>% count(USAGE_OF_BACKING_FILM_max)


# variables ---------------------------------------------------------------

WAFER_ID
STAGE
AVG_REMOVAL_RATE
AVG_REMOVAL_RATE_OUTLIER_FLAG
STATION
MULTIPLE_STAGE_FLAG
TOTAL_PROCESSING_DURATION
A_1
A_2
A_3
A_4
A_5
A_6
B_4
B_5
B_6
SEQUENCE
TOTAL_POLISHING_DURATION
PRCT_TIME_POLISHING
BACKING_FILM_REPLACED_FLAG_max
BACKING_FILM_REPLACED_FLAG_mean
BACKING_FILM_REPLACED_FLAG_min
BACKING_FILM_REPLACED_FLAG_sum
CENTER_AIR_BAG_PRESSURE_max
CENTER_AIR_BAG_PRESSURE_mean
CENTER_AIR_BAG_PRESSURE_min
CENTER_AIR_BAG_PRESSURE_sum
DRESSER_REPLACED_FLAG_max
DRESSER_REPLACED_FLAG_mean
DRESSER_REPLACED_FLAG_min
DRESSER_REPLACED_FLAG_sum
DRESSING_WATER_STATUS_max
DRESSING_WATER_STATUS_mean
DRESSING_WATER_STATUS_min
DRESSING_WATER_STATUS_sum
EDGE_AIR_BAG_PRESSURE_max
EDGE_AIR_BAG_PRESSURE_mean
EDGE_AIR_BAG_PRESSURE_min
EDGE_AIR_BAG_PRESSURE_sum
HEAD_ROTATION_max
HEAD_ROTATION_mean
HEAD_ROTATION_min
HEAD_ROTATION_sum
MAIN_OUTER_AIR_BAG_PRESSURE_max
MAIN_OUTER_AIR_BAG_PRESSURE_mean
MAIN_OUTER_AIR_BAG_PRESSURE_min
MAIN_OUTER_AIR_BAG_PRESSURE_sum
MEMBRANE_REPLACED_FLAG_max
MEMBRANE_REPLACED_FLAG_mean
MEMBRANE_REPLACED_FLAG_min
MEMBRANE_REPLACED_FLAG_sum
POLISHING_TABLE_REPLACED_FLAG_max
POLISHING_TABLE_REPLACED_FLAG_mean
POLISHING_TABLE_REPLACED_FLAG_min
POLISHING_TABLE_REPLACED_FLAG_sum
PRESSURIZED_CHAMBER_PRESSURE_max
PRESSURIZED_CHAMBER_PRESSURE_mean
PRESSURIZED_CHAMBER_PRESSURE_min
PRESSURIZED_CHAMBER_PRESSURE_sum
PRESSURIZED_SHEET_REPLACED_FLAG_max
PRESSURIZED_SHEET_REPLACED_FLAG_mean
PRESSURIZED_SHEET_REPLACED_FLAG_min
PRESSURIZED_SHEET_REPLACED_FLAG_sum
RETAINER_RING_PRESSURE_max
RETAINER_RING_PRESSURE_mean
RETAINER_RING_PRESSURE_min
RETAINER_RING_PRESSURE_sum
RIPPLE_AIR_BAG_PRESSURE_max
RIPPLE_AIR_BAG_PRESSURE_mean
RIPPLE_AIR_BAG_PRESSURE_min
RIPPLE_AIR_BAG_PRESSURE_sum
SLURRY_FLOW_LINE_A_max
SLURRY_FLOW_LINE_A_mean
SLURRY_FLOW_LINE_A_min
SLURRY_FLOW_LINE_A_sum
SLURRY_FLOW_LINE_B_max
SLURRY_FLOW_LINE_B_mean
SLURRY_FLOW_LINE_B_min
SLURRY_FLOW_LINE_B_sum
SLURRY_FLOW_LINE_C_max
SLURRY_FLOW_LINE_C_mean
SLURRY_FLOW_LINE_C_min
SLURRY_FLOW_LINE_C_sum
STAGE_ROTATION_max
STAGE_ROTATION_mean
STAGE_ROTATION_min
STAGE_ROTATION_sum
TIMESTAMP_max
TIMESTAMP_mean
TIMESTAMP_min
TIMESTAMP_sum
USAGE_OF_BACKING_FILM_max
USAGE_OF_BACKING_FILM_mean
USAGE_OF_BACKING_FILM_min
USAGE_OF_BACKING_FILM_sum
USAGE_OF_DRESSER_max
USAGE_OF_DRESSER_mean
USAGE_OF_DRESSER_min
USAGE_OF_DRESSER_sum
USAGE_OF_DRESSER_TABLE_max
USAGE_OF_DRESSER_TABLE_mean
USAGE_OF_DRESSER_TABLE_min
USAGE_OF_DRESSER_TABLE_sum
USAGE_OF_MEMBRANE_max
USAGE_OF_MEMBRANE_mean
USAGE_OF_MEMBRANE_min
USAGE_OF_MEMBRANE_sum
USAGE_OF_POLISHING_TABLE_max
USAGE_OF_POLISHING_TABLE_mean
USAGE_OF_POLISHING_TABLE_min
USAGE_OF_POLISHING_TABLE_sum
USAGE_OF_PRESSURIZED_SHEET_max
USAGE_OF_PRESSURIZED_SHEET_mean
USAGE_OF_PRESSURIZED_SHEET_min
USAGE_OF_PRESSURIZED_SHEET_sum
WAFER_ROTATION_max
WAFER_ROTATION_mean
WAFER_ROTATION_min
WAFER_ROTATION_sum
STAGE_DURATION


# response ----------------------------------------------------------------

# box plots -----
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = STATION, y = AVG_REMOVAL_RATE)) + 
  geom_boxplot()

# boxplot with outliers
ggplot(train_df, 
       aes(x = STATION, y = AVG_REMOVAL_RATE)) + 
  geom_boxplot()

# univariate correlation --------------------------------------------------

# durations

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = as.factor(MULTIPLE_STAGE_FLAG), y = AVG_REMOVAL_RATE)) + 
  geom_boxplot() +
  facet_wrap(~ STATION)

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TOTAL_PROCESSING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION)

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TOTAL_POLISHING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION)

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = PRCT_TIME_POLISHING, y = AVG_REMOVAL_RATE)) + 
  geom_jitter() +
  facet_wrap(~ STATION)

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = STAGE_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION)

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_DRESSER_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION)

# usage ----------

# scatterplots
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_BACKING_FILM_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_DRESSER_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_MEMBRANE_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_POLISHING_TABLE_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = USAGE_OF_PRESSURIZED_SHEET_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

# measurements ---------

# outliers of processing duration
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TOTAL_PROCESSING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = A_1, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

# outliers of polishing duration
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TOTAL_POLISHING_DURATION, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = CENTER_AIR_BAG_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df,
         #mutate(cluster = kmeans(CENTER_AIR_BAG_PRESSURE_max_P123, 6)$cluster), 
       aes(x = CENTER_AIR_BAG_PRESSURE_max_P123, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION, scales = "free_x") +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = EDGE_AIR_BAG_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

# outliers of head rotation (less than 90)
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = HEAD_ROTATION_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = MAIN_OUTER_AIR_BAG_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = PRESSURIZED_CHAMBER_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = RETAINER_RING_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = RIPPLE_AIR_BAG_PRESSURE_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = SLURRY_FLOW_LINE_A_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = SLURRY_FLOW_LINE_B_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = SLURRY_FLOW_LINE_C_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = STAGE_ROTATION_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = WAFER_ROTATION_mean, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

# positive correlation
ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = TIMESTAMP_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth()

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")

ggplot(train_df %>% filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = , y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ STATION) +
  geom_smooth(method = "lm")


# facets ------------------------------------------------------------------

ggplot(train_df_long,
       aes(x = val)) +
  geom_histogram() +
  facet_wrap( ~ var, scales = "free") +
  theme_grey(8) +
  ggsave("figures/usage_by_station.png", device = "png", width = 6, height = 8)


# missing data ------------------------------------------------------------

ggplot(train_df, aes(x = CENTER_AIR_BAG_PRESSURE_max_P123)) +
  geom_histogram() +
  facet_wrap(~ STATION)

ggplot(train_df, aes(x = RIPPLE_AIR_BAG_PRESSURE_max_P123)) +
  geom_histogram() +
  facet_wrap(~ STATION)

ggplot(train_df, aes(x = SLURRY_FLOW_LINE_A_max_P123)) +
  geom_histogram() +
  facet_wrap(~ STATION)

ggplot(train_df, aes(x = USAGE_OF_DRESSER_max)) +
  geom_histogram() +
  facet_wrap(~ STATION)

# dresser wear ------------------------------------------------------------

train_df %>% count(JOB_WITHIN_CPP)
ggplot(train_df, aes(x = as.factor(JOB_WITHIN_CPP))) + geom_bar()
train_df %>% count(JOB_WITHIN_CPP, OVERALL_CPP) %>% filter(JOB_WITHIN_CPP == 10)
train_df %>% count(JOB_WITHIN_CPP, OVERALL_CPP) %>% filter(JOB_WITHIN_CPP == 9)
train_df %>% count(JOB_WITHIN_CPP, OVERALL_CPP) %>% filter(JOB_WITHIN_CPP == 8)
train_df %>% count(JOB_WITHIN_CPP, OVERALL_CPP) %>% filter(JOB_WITHIN_CPP == 7)
train_df %>% count(JOB_WITHIN_CPP, OVERALL_CPP) %>% filter(JOB_WITHIN_CPP == 6)

long_cpp <- train_df %>% 
  count(JOB_WITHIN_CPP, OVERALL_CPP) %>% 
  filter(JOB_WITHIN_CPP == 6) %>%
  .$OVERALL_CPP

train_df %>% filter(JOB_WITHIN_CPP == 10) %>% distinct(WAFER_ID, STAGE)

ggplot(train_df %>%
         filter(OVERALL_CPP %in% long_cpp), 
       aes(x = TIMESTAMP_min, y = AVG_REMOVAL_RATE)) + 
  geom_line() +
  facet_wrap(~ OVERALL_CPP, scales = "free")
