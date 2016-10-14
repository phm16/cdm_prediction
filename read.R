# library(tidyverse)

train_response <- read_csv("data/CMP-training-removalrate.csv")

train_files <- list.files("data/training")

col_types_list <- 
  list(
    MACHINE_ID = col_integer(),
    MACHINE_DATA = col_integer(),
    TIMESTAMP = col_double(),
    WAFER_ID = col_double(),
    STAGE = col_character(),
    CHAMBER = col_double(),
    USAGE_OF_BACKING_FILM = col_double(),
    USAGE_OF_DRESSER = col_double(),
    USAGE_OF_POLISHING_TABLE = col_double(),
    USAGE_OF_DRESSER_TABLE = col_double(),
    PRESSURIZED_CHAMBER_PRESSURE = col_double(),
    MAIN_OUTER_AIR_BAG_PRESSURE = col_double(),
    CENTER_AIR_BAG_PRESSURE = col_double(),
    RETAINER_RING_PRESSURE = col_double(),
    RIPPLE_AIR_BAG_PRESSURE = col_double(),
    USAGE_OF_MEMBRANE = col_double(),
    USAGE_OF_PRESSURIZED_SHEET = col_double(),
    SLURRY_FLOW_LINE_A = col_double(),
    SLURRY_FLOW_LINE_B = col_double(),
    SLURRY_FLOW_LINE_C = col_double(),
    WAFER_ROTATION = col_double(),
    STAGE_ROTATION = col_double(),
    HEAD_ROTATION = col_double(),
    DRESSING_WATER_STATUS = col_double(),
    EDGE_AIR_BAG_PRESSURE = col_double()
  )

train_df <- map_df(train_files, function(file) {
  read_csv(paste0("data/training/", file), col_types = col_types_list)
  }
) %>% inner_join(train_response, by = c("WAFER_ID", "STAGE"))

rm(list = ls()[!(ls() %in% c('train_response', 'train_df'))])

# write_rds(train_df, "phm_dashboard/data/train_df.rds")
