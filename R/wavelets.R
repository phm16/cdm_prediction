library(wavelets)

# expore the use of wavelets for feature extraction

wvlt <- train_df %>% filter(WAFER_ID == 2939014292, STAGE == "A") %>%
  #.$WAFER_ROTATION %>%
  .$STAGE_ROTATION %>%
  dwt(., filter = "haar", n.levels = 1)

plot(wvlt)

plot(wvlt@W$W1) # mean(wvlt@W$W1)
plot(wvlt@V$V1) # mean(wvlt@V$V1)
plot(wvlt@series)

str(wvlt)

wvlt_df <- data.frame(
  X = seq_along(wvlt@W$W1),
  W1 = wvlt@W$W1,
  V1 = wvlt@V$V1
)

ggplot(wvlt_df %>% gather(var, val, -X), aes(x = X, y = val, color = var)) + geom_line()

wvlt_agrgt <- wvlt_df %>% 
  mutate(V1_DIFF = V1 - lag(V1, 1, default = 0),
         AREA = sum(V1_DIFF)) %>%
  summarize(INTGRL = sum(AREA))