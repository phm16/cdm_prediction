# principal components ----------------------------------------------------

pressure_prcomp <- mdl_df %>% select(contains("PRESSURE")) %>% 
  mutate_all(.funs = scale) %>% 
  prcomp(.) %>%
  predict(., mdl_df) %>%
  data.frame(.)

mdl_df_prcomp <- mdl_df %>% bind_cols(pressure_prcomp)