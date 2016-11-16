library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(caret)

train_df <- readRDS('data/train_df.rds')

ctrl <-
  trainControl(
    method = "cv",
    number = 5,
    allowParallel = TRUE,
    savePredictions = "final"
  )

set.seed(59)
glm_mdl_2 <- train(
  AVG_REMOVAL_RATE ~ .,
  data = train_df %>% filter(!STATION == "A123") %>%
    select(
      AVG_REMOVAL_RATE,
      AVG_REMOVAL_RATE_lag1,
      STATION,
      CENTER_AIR_BAG_PRESSURE_mean_P123,
      PRESSURIZED_CHAMBER_PRESSURE_mean_P123,
      SLURRY_FLOW_LINE_A_mean_P123,
      SLURRY_FLOW_LINE_C_mean_P123,
      USAGE_OF_DRESSER_max_P123,
      USAGE_OF_DRESSER_TABLE_max_P123,
      HEAD_ROTATION_mean_P123,
      WAFER_ROTATION_mean_P123
    ),
  method = "glm",
  trControl = ctrl
)

timestamp_list <- train_df %>% distinct(TIMESTAMP_min)

wafer_id_list <- train_df %>% distinct(WAFER_ID) %>% arrange(WAFER_ID) %>% .$WAFER_ID

var_list <- names(train_df %>% select(-WAFER_ID))

station_list <- c("A456", "B456")

head_rotation_list <- c(160L, 192L)

wafer_rotation_list <- c(23L, 35L)

last_vals <- train_df %>%
  filter(TIMESTAMP_min == max(train_df$TIMESTAMP_min))

dresser_life <- 1 - (last_vals$USAGE_OF_DRESSER_max_P123 / 700)

backing_film_life <- 1 - (last_vals$USAGE_OF_BACKING_FILM_max_P123 / 10000)

ui <- dashboardPage(
  dashboardHeader(title = "Wafer Polishing Process"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Monitoring", tabName = "dashboard", icon = icon("dashboard")),
      
      selectInput("station", "Station", station_list, selected = "A456"),
      
      sliderInput("cabp", "Center Air Bag Pressure", min = 20, max = 100, value = 60),
      
      sliderInput("pcp", "Pressurized Chamber Pressure", min = 20, max = 100, value = 60),
      
      sliderInput("slurryA", "Slurry Flow Rate Line A", min = 0, max = 16, value = 8),
      
      sliderInput("slurryC", "Slurry Flow Rate Line C", min = 50, max = 450, value = 250)
      
    ), width = 300
    
    ),
  dashboardBody(
    
    fluidRow(
      valueBox(paste0(round(dresser_life, 2) * 100, '%'), "Dresser Life Remaining", color = "red", icon = icon("exclamation-triangle")),
      valueBox(paste0(round(backing_film_life, 2) * 100, '%'), "Backing Film Life Remaining", color = "green"),
      box(plotOutput("avg_removal_rate_plot"), width = 9)
      )
    )
  )

server <- function(input, output) {
  
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
  
  plot_df <- reactive({
    
    train_df_station <- train_df %>%
      filter(STATION == input$station)
    
  })
      
  cabp_intercept <- reactive({
    
    data.frame(Value = input$cabp)
    
    })
  
  output$avg_removal_rate_plot <- renderPlot({
    
    ggplot(plot_df(), aes(x = AVG_REMOVAL_RATE)) +
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
  
}

shinyApp(ui, server)