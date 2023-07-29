
ui <- fluidPage(
  
  titlePanel("CO2 Emissions By Type"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("country", "Country:", c("United States" = "USA",
                                            "China" = "CHN",
                                            "India" = "IND")),
      
      
      br(),
      
      sliderInput(
        inputId = "year",
        label = "Year", min = 1950, max = 2019, value = 2000,
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("co2_table"), textOutput("table_message")),
                  tabPanel("Visualization", plotOutput("co2_plot"), textOutput("plot_message"))
      )
    )
  )
  
  
  
  
  
)