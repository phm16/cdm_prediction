

function(input, output) {
  
  current_timestamp <- reactive({
    
    train_df %>%
      filter(STATION == input$station) %>%
      filter(TIMESTAMP_min == input$ts) %>%
      select(TIMESTAMP_min, AVG_REMOVAL_RATE)
    
  })
  
  prediction <- reactive({data.frame(
    AVG_REMOVAL_RATE_lag1 = last_vals$AVG_REMOVAL_RATE_lag1,
    STATION = input$station,
    CENTER_AIR_BAG_PRESSURE_mean_P123 = input$cabp,
    PRESSURIZED_CHAMBER_PRESSURE_mean_P123 = input$pcp,
    SLURRY_FLOW_LINE_A_mean_P123 = input$slurryA,
    SLURRY_FLOW_LINE_C_mean_P123 = input$slurryC,
    USAGE_OF_DRESSER_max_P123 = last_vals$USAGE_OF_DRESSER_max_P123,
    USAGE_OF_DRESSER_TABLE_max_P123 = last_vals$USAGE_OF_DRESSER_TABLE_max_P123,
    HEAD_ROTATION_mean_P123 = 160L,
    WAFER_ROTATION_mean_P123 = 12L
  ) %>% predict(glm_mdl_2, .) %>% data.frame(AVG_REMOVAL_RATE = .)
    
  })
  
  #prediction <- predict(glm_mdl_2, predict_df) %>% .[[1]]
  
  rr_df <- reactive({
    
    train_df %>%
      filter(STATION == input$station)
    
  })
  
  cabp_intercept <- reactive({
    
    data.frame(Value = input$cabp)
    
  })
  
  train_df_ts <- reactive({
    
    train_df_ts_last_10_jobs %>%
      filter(WAFER_ID == input$wafer_id) %>%
      filter(STATION == input$station) %>%
      select(-MACHINE_ID, -MACHINE_DATA) %>%
      gather(var, val, -WAFER_ID, -STAGE, -POLISH_TYPE, -POLISH_PHASE, -STATION, -TIMESTAMP, -CHAMBER) %>%
      group_by(var) %>%
      mutate(scaled_val = scale(val)) %>%
      ungroup() %>%
      filter(var %in% input$vars) 
    
  })
  
  yield_curves_labels <- reactive({
    
    yield_curves %>% filter(absolute_error == input$abs_error)
    
  })
  
  output$avg_removal_rate_plot <- renderPlot({
    
    ggplot(rr_df(), aes(x = AVG_REMOVAL_RATE)) +
      geom_histogram(fill = "grey") +
      geom_vline(xintercept = prediction()$AVG_REMOVAL_RATE, color = "dodgerblue", linetype = "dashed", size = 1) +
      labs(x = "Removal Rate", y = "", title = "Predicted Removal Rate") +
      geom_text(data = prediction(), 
                aes(y = 5, label = round(AVG_REMOVAL_RATE, 0)), 
                color = "dodgerblue", 
                nudge_x = 2, 
                size = 8
      )
    
    
  })
  
  output$ts_plot <- renderPlot({
    
    ggplot(train_df_ts(), aes(x = TIMESTAMP, y = scaled_val, color = var)) +
      geom_line()
    
  })
 
  output$yield_plot <- renderPlot({
    
    ggplot(
      yield_curves,
      aes(x = absolute_error,
          y = yield,
          color = model
      )) + geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      geom_vline(xintercept = input$abs_error, linetype = "dashed") +
      geom_label(data = yield_curves_labels(),
                 aes(x = absolute_error, 
                     label = paste0(round(yield, 3) * 100, " %"), color = model)) +
      labs(title = "Yield vs. Acceptable Error", x = "Removal Rate Tolerance", y = "Yield", color = "Model")
    
  })
  
}