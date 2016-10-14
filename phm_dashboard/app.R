library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(caret)
library(lubridate)

train_df <- readRDS("data/train_df.rds")

wafer_id_list <- train_df %>% distinct(WAFER_ID) %>% arrange(WAFER_ID) %>% .$WAFER_ID

var_list <- names(train_df %>% select(-WAFER_ID))

ui <- dashboardPage(
  dashboardHeader(title = "Wafer Polishing Process"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Monitoring", tabName = "dashboard", icon = icon("dashboard")),
      
      selectInput("wafer_id", "Wafer ID:", wafer_id_list),
      
      selectInput("variable", "Variable:", var_list, selected = "CENTER_AIR_BAG_PRESSURE")
      
    ), width = 300
    
    ),
  dashboardBody(
    
    fluidRow(
      
      box(plotOutput("plot"))
    )
    
    
    )
)

server <- function(input, output) {
  
  pd <- reactive({
    train_df[train_df$WAFER_ID == input$wafer_id, c("TIMESTAMP", input$variable)]
  })
  
  output$plot <- renderPlot({
    
    ggplot(pd(), aes_string("TIMESTAMP", y = input$variable)) + geom_line(color = "dodgerblue1")
    
    
  })
}

shinyApp(ui, server)
