

# sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem(
      "Monitoring",
      tabName = "monitoring",
      icon = icon("line-chart")
    ),
    
    menuItem(
      "Removal Rate Tuning",
      tabName = "tuning",
      icon = icon("dashboard")
    ),
    
    menuItem(
      "Yield Curves",
      tabName = "yield",
      icon = icon("line-chart")
    ),
    
    selectInput("station", "Station", station_list, selected = "A456")
  ),
  width = 300
  
)

# body --------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tuning",
            
    fluidRow(
      box(
        
        sliderInput(
          "cabp",
          "Center Air Bag Pressure",
          min = 20,
          max = 100,
          value = 60
        ),
        
        sliderInput(
          "pcp",
          "Pressurized Chamber Pressure",
          min = 20,
          max = 100,
          value = 60
        )
        
      ),
      
      box(
        
        sliderInput(
          "slurryA",
          "Slurry Flow Rate Line A",
          min = 0,
          max = 16,
          value = 8
        ),
        
        sliderInput(
          "slurryC",
          "Slurry Flow Rate Line C",
          min = 50,
          max = 450,
          value = 250
        )
        
      )
      
    ),
    
    fluidRow(box(plotOutput("avg_removal_rate_plot"), width = 8))
  ),
  
  tabItem(
    tabName = "monitoring",
    fluidRow(

      box(
      selectInput("wafer_id", label = "Wafer ID", 
                  choices = wafer_ids, 
                  selected = 29494154)
      ),
      
      box(
      
        selectInput("vars", label = "Variables", 
                           choices = list(
                             "CENTER_AIR_BAG_PRESSURE",
                             "PRESSURIZED_CHAMBER_PRESSURE",
                             "SLURRY_FLOW_LINE_A",
                             "SLURRY_FLOW_LINE_B",
                             "SLURRY_FLOW_LINE_C",
                             "WAFER_ROTATION",
                             "STAGE_ROTATION"
                             
                             ),
                           selected = "CENTER_AIR_BAG_PRESSURE",
                    multiple = TRUE,
                    selectize = FALSE
                    
                    )
        
      )),

    
    fluidRow(
      box(width = 8, plotOutput("ts_plot"))
    )
      
      
    
    
    ),
  
  tabItem(
    tabName = "yield",
    
    fluidRow(
      box(
      
        sliderInput(
          "abs_error",
          "Error Tolerance",
          min = 1,
          max = 40,
          value = 10
        )
        
        
        )
    ),
    
    fluidRow(
      box(width = 8, plotOutput("yield_plot"))
    )
  )
  
  )
)


# notifications -----------------------------------------------------------

header <- dashboardHeader(title = "Wafer Polishing Process",

# fluidRow(
#   valueBox(paste0(round(dresser_life, 2) * 100, '%'), "Dresser Life Remaining", color = "red", icon = icon("exclamation-triangle"), width = 6),
#   valueBox(paste0(round(backing_film_life, 2) * 100, '%'), "Backing Film Life Remaining", color = "green", width = 6)
#   
# ),
                          
dropdownMenu(
  type = "tasks", badgeStatus = "danger",
             taskItem(value = 94, color = "red",
                      "Dresser Life Consumed"
             ),
             taskItem(value = 6, color = "green",
                      "Backing Film Life Consumed"
             )
))

# page --------------------------------------------------------------------

dashboardPage(
  header,
  sidebar,
  body
)