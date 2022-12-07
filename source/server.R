# server.R
library(dplyr)
library(shiny)
library(plotly)


# chart number two
server <- shinyServer(
  function(input, output) {
    # assign values to `output` here
    merge_df <- reactive({
      req(input$year)
      react_df <- merge %>% filter(year %in% input$year) %>% group_by(country_type) %>% summarise(percent = mean(percent)) 
    })
    
    output$plot <- renderPlot({
      ggplot(data = merge_df(), aes_string(x = "country_type", y = "percent", fill = 'country_type')) + 
        geom_bar(stat = "identity", width = 0.5) + labs(x = "Country Type", y = "Percent of country with access to handwashing")
  
    })
  }
)
