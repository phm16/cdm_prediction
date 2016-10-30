
# response id is WAFER_ID + STAGE
# MACHINE_ID is all 2
# MACHINE_DATA has 6 vals
# MACHINE_DATA == CHAMBER
# huge gap in timestamp: 2921014258, -4019511766
# reprocessed:  -4019511766
# usage of backing film (max) good predictor for outliers 
# max timestamp:  487268210
# min timestamp:  481634410
# 70% timestamp:  485578070 min(base_df$TIMESTAMP) + 0.7 * (max(base_df$TIMESTAMP) - min(base_df$TIMESTAMP))

glimpse(base_df)

# Observations: 672,744
# Variables: 26
# $ MACHINE_ID                   <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,...
# $ MACHINE_DATA                 <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
# $ TIMESTAMP                    <dbl> 481637082, 481637083, 481637085, 481637086, 481637086, 4816370...
# $ WAFER_ID                     <dbl> 371447024, 371447024, 371447024, 371447024, 371447024, 3714470...
# $ STAGE                        <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A...
# $ CHAMBER                      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
# $ USAGE_OF_BACKING_FILM        <dbl> 9302.500, 9302.500, 9302.500, 9302.500, 9302.500, 9302.500, 93...
# $ USAGE_OF_DRESSER             <dbl> 534.0741, 534.0741, 534.0741, 534.0741, 534.0741, 534.0741, 53...
# $ USAGE_OF_POLISHING_TABLE     <dbl> 292.5926, 292.5926, 292.5926, 292.5926, 292.5926, 292.5926, 29...
# $ USAGE_OF_DRESSER_TABLE       <dbl> 2666.25, 2666.25, 2666.25, 2666.25, 2666.25, 2666.25, 2666.25,...
# $ PRESSURIZED_CHAMBER_PRESSURE <dbl> 67.14286, 67.14286, 67.14286, 66.66667, 67.14286, 67.14286, 67...
# $ MAIN_OUTER_AIR_BAG_PRESSURE  <dbl> 346.8, 348.0, 346.8, 348.0, 348.0, 348.0, 346.8, 348.0, 346.8,...
# $ CENTER_AIR_BAG_PRESSURE      <dbl> 101.875, 101.875, 101.875, 101.875, 101.875, 101.875, 101.875,...
# $ RETAINER_RING_PRESSURE       <dbl> 1739.4, 1743.3, 1743.3, 1739.4, 1735.5, 1743.3, 1739.4, 1739.4...
# $ RIPPLE_AIR_BAG_PRESSURE      <dbl> 15.22727, 15.22727, 15.22727, 15.27273, 15.27273, 15.22727, 15...
# $ USAGE_OF_MEMBRANE            <dbl> 110.3063, 110.3063, 110.3063, 110.3063, 110.3063, 110.3063, 11...
# $ USAGE_OF_PRESSURIZED_SHEET   <dbl> 2790.75, 2790.75, 2790.75, 2790.75, 2790.75, 2790.75, 2790.75,...
# $ SLURRY_FLOW_LINE_A           <dbl> 2.222222, 2.222222, 2.222222, 2.222222, 2.222222, 2.222222, 2....
# $ SLURRY_FLOW_LINE_B           <dbl> 0.9090909, 0.9090909, 0.9090909, 0.9090909, 0.9090909, 0.90909...
# $ SLURRY_FLOW_LINE_C           <dbl> 565.6, 568.4, 568.4, 568.4, 571.2, 571.2, 574.0, 576.8, 576.8,...
# $ WAFER_ROTATION               <dbl> 34.65116, 34.65116, 34.65116, 34.65116, 34.65116, 34.65116, 34...
# $ STAGE_ROTATION               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
# $ HEAD_ROTATION                <dbl> 156.8, 156.8, 156.8, 156.8, 156.8, 156.8, 156.8, 156.8, 156.8,...
# $ DRESSING_WATER_STATUS        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
# $ EDGE_AIR_BAG_PRESSURE        <dbl> 60.90909, 60.90909, 60.90909, 60.60606, 60.90909, 60.60606, 60...
# $ AVG_REMOVAL_RATE             <dbl> 149.1309, 149.1309, 149.1309, 149.1309, 149.1309, 149.1309, 14...

# long data frame ---------------------------------------------------------

base_df_long <- base_df %>% gather(var, val, -WAFER_ID, -STAGE, -STATION, -TIMESTAMP, -CHAMBER) %>%
  filter(!var %in% c("MACHINE_ID", "MACHINE_DATA"))

# response ----------------------------------------------------------------

ggplot(response,
       aes(x = AVG_REMOVAL_RATE)) + 
  geom_histogram()

ggplot(response %>% 
         filter(AVG_REMOVAL_RATE >= 1000), 
       aes(x = AVG_REMOVAL_RATE)) + 
  geom_histogram()

ggplot(response %>% 
         filter(AVG_REMOVAL_RATE < 1000), 
       aes(x = AVG_REMOVAL_RATE)) + 
  geom_histogram()


# training observations count ---------------------------------------------

# check the number of observations per job
base_df %>% 
  count(WAFER_ID, STAGE) %>% 
  ggplot(., aes(x = n)) + geom_histogram()

# there are some jobs with significantly more data than others
base_df %>%
  count(WAFER_ID, STAGE) %>% 
  filter(n < 1000) %>%
  ggplot(., aes(x = n)) + geom_histogram()

base_df %>% 
  count(WAFER_ID, STAGE) %>% 
  .$n %>%
  quantile(.)


# time series plots -------------------------------------------------------

# avg removal rate vs. time
ggplot(base_df, 
       aes(
         x = TIMESTAMP,
         y = AVG_REMOVAL_RATE
       )) + 
  geom_line()

# exclude the outliers
ggplot(base_df %>% 
         filter(AVG_REMOVAL_RATE < 1000), 
       aes(
         x = TIMESTAMP,
         y = AVG_REMOVAL_RATE,
         color = STAGE
       )) + 
  geom_point(alpha = 0.5)


# single series -----------------------------------------------------------

ggplot(base_df %>% 
         filter(WAFER_ID == 371447024, STAGE == "A"),
       aes(x = TIMESTAMP, y = CENTER_AIR_BAG_PRESSURE, color = as.factor(CHAMBER))) +
  geom_line() +
  labs(title = "WAFER_ID 371447024 - STAGE A", x = "TIMESTAMP", color = "CHAMBER") #+
  #geom_hline(yintercept = 85.18575, linetype = "dashed")

# stage and chamber -------------------------------------------------------

# identify outliers in stage / chamber durations
# outliers are above ~ 1000
stage_chamber_durations %>%
  gather(STAGE_CHAMBER, DURATION, -WAFER_ID, -SEQUENCE) %>%
  filter(DURATION > 0, DURATION < 1000) %>%
  ggplot(., aes(x = STAGE_CHAMBER, y = DURATION)) +
  geom_boxplot()

# quantiles
quantile(base_df$TIMESTAMP, probs = seq(0, 1, 0.1))

# look at subset of time series - facets
ggplot(base_df %>% 
         filter(AVG_REMOVAL_RATE < 1000, TIMESTAMP <= 481985972),
       aes(
         x = TIMESTAMP,
         y = AVG_REMOVAL_RATE,
         color = STAGE
       )) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~ STAGE + CHAMBER) +
  geom_smooth()

# look at subset of time series - colors
ggplot(base_df %>% 
         filter(AVG_REMOVAL_RATE < 1000, TIMESTAMP <= 481985972) %>%
         mutate(STAGE_CHAMBER = paste0(STAGE, "_", CHAMBER)),
       aes(
         x = TIMESTAMP,
         y = AVG_REMOVAL_RATE,
         color = STAGE_CHAMBER
       )) + 
  geom_point(alpha = 0.5) +
  geom_smooth()

ggplot(base_df %>%
         unite(STAGE_CHAMBER, STAGE, CHAMBER) %>%
         filter(WAFER_ID == -4230160598),
       aes(
         x = TIMESTAMP,
         y = AVG_REMOVAL_RATE,
         color = STAGE_CHAMBER
       )) +
  geom_point()

# aggregate time series ---------------------------------------------------

base_df_agrgt <- base_df %>%
  mutate(TIMESTAMP2 = floor(TIMESTAMP / 1000) * 1000) %>%
  select_if(is.numeric) %>%
  group_by(TIMESTAMP2) %>%
  summarize_all(mean) %>%
  filter(AVG_REMOVAL_RATE < 1000)

ggplot(base_df_agrgt,
  aes(
    x = TIMESTAMP2, 
    y = AVG_REMOVAL_RATE
  )) +
    geom_point(alpha = 0.5)


# job plots ---------------------------------------------------------------

ggplot(base_df_long %>% filter(WAFER_ID == 371447024, STAGE == "A"),
       aes(x = TIMESTAMP, y = val, color = as.factor(CHAMBER))) +
  geom_line() +
  facet_wrap(~ var, scales = "free")

ggplot(base_df_long %>% filter(WAFER_ID == -4019511766),
       aes(x = TIMESTAMP, y = val, color = as.factor(CHAMBER))) +
  geom_line() +
  facet_wrap(~ var, scales = "free")

wafer_ids <- unique(response$WAFER_ID)

ggplot(base_df_long %>% filter(WAFER_ID == 2939014292, STAGE == "A"),
       aes(x = TIMESTAMP, y = val, color = as.factor(CHAMBER))) +
  geom_line() +
  facet_wrap(~ var, scales = "free") +
  #ggtitle("WAFER_ID: 371447024, STAGE: A") +
  theme_grey(base_size = 8)

lapply(wafer_ids[1:10], function(wafer_id) {

  title <- wafer_id
  
  ggplot(base_df_long %>% filter(WAFER_ID == wafer_id),
         aes(x = TIMESTAMP, y = val, color = STAGE)) +
    geom_line() +
    facet_wrap(~ var, scales = "free") +
    ggtitle(paste0(title)) +
    theme_grey(base_size = 8) #+
    ggsave(paste0("figures/individual/", title, ".pdf"), device = "pdf", width = 8, height = 6)
  
  })

# usage -------------------------------------------------------------------

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_BACKING_FILM)) + 
  geom_line()

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_DRESSER)) + 
  geom_line()

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_POLISHING_TABLE)) + 
  geom_line()

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_DRESSER_TABLE)) + 
  geom_line()

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_MEMBRANE)) + 
  geom_line()

ggplot(base_df, 
       aes(x = TIMESTAMP, y = USAGE_OF_PRESSURIZED_SHEET)) + 
  geom_line()


# individual variables ----------------------------------------------------

ggplot(base_df %>% filter(WAFER_ID == 371447024, STAGE == "A"), 
       aes(x = TIMESTAMP, y = RETAINER_RING_PRESSURE, color = as.character(CHAMBER))) + 
  geom_line()


# center air bag pressure -------------------------------------------------

ggplot(
  base_df %>%
    filter(AVG_REMOVAL_RATE < 1000),
  aes(x = CENTER_AIR_BAG_PRESSURE_max, y = AVG_REMOVAL_RATE)) + 
  geom_point() +
  facet_wrap(~ SEQUENCE)



# rework ------------------------------------------------------------------

ggplot(base_df %>% filter(WAFER_ID == -4019511766),
       aes(x = TIMESTAMP, y = CHAMBER, color = STAGE)) +
  geom_point() +
  geom_vline(xintercept = 483966563)


# facet plots -------------------------------------------------------------

ggplot(base_df_long %>%
         filter(!substr(var, 1, 5) == "USAGE"),
       aes(x = val)) +
  geom_histogram() +
  facet_wrap( ~ var, scales = "free") +
  theme_grey(8)
