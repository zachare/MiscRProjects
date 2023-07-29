library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)


owid_co2_data <- read.csv('./data/owid-co2-data.csv', stringsAsFactors = FALSE)

server <- function(input, output) {
  output$co2_table <- renderTable({
    table <- owid_co2_data %>%
      filter(iso_code == input$country) %>%
      filter(year == input$year) %>%
      select(cement_co2	, coal_co2, gas_co2, oil_co2, other_industry_co2)
    
    width = "auto"
    
    colnames(table) <- c("From Cement", "From Coal", "From Gas", "From Oil", "From Other Industries") 
    table
  })
  
  output$co2_plot <- renderPlot({
    plot <- owid_co2_data %>%
      filter(iso_code == input$country) %>%
      filter(year == input$year) %>%
      select(cement_co2, coal_co2, gas_co2, oil_co2, other_industry_co2) %>%
      rename("Cement" = cement_co2, "Coal" = coal_co2, "Gas" = gas_co2, "Oil" = oil_co2, "Other" = other_industry_co2) %>%
      pivot_longer(everything(), names_to = "types", values_to = "value") %>%
      ggplot(aes(x="", y=value, fill=types)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() + 
      labs(title = "CO2 Emissions By Type", fill = "Type" )
      
    plot
    
  })
  
  output$table_message <- renderText({
    message <- "This table shows the data for "
    message_str <- paste0(message, input$year, ".")
    message_str
  })
  
  output$plot_message <- renderText({
    message <- "This plot shows the data for "
    message_str <- paste0(message, input$year, ".")
    message_str
  })
  
  
  }