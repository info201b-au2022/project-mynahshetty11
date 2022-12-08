# server.R
library(dplyr)
library(shiny)
library(plotly)
library(ggplot2)

options(scipen = 100)
poverty <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-mynahshetty11/main/data/data_poverty.csv")
handwash <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-mynahshetty11/main/data/data_handwash.csv")
# chart number two
merge <- na.omit(merge)

server <- shinyServer(
  function(input, output) {
# assign values to `output` here
    merge_df <- reactive({
      req(input$Year)
      react_df <- merge %>% filter(Year %in% input$Year) %>% group_by(country_type) %>% summarise(Percent = (mean((Percent)))) 
    })
    output$chart <- renderPlot({
       ggplot(data = merge_df(), aes_string(x = "country_type", y = "Percent", fill = 'country_type')) + 
        geom_bar(stat = "identity", width = 0.5) + labs(x = "Country Type", y = "Percent of Countries with Access to Handwashing", fill = "Country Type") +  
        labs(title = "Percent of Handwashing Availibility in types of Countries Per Year",
      caption = "Caption: This depicts the average proportion per year of 
      handwashing in countries based on their classification of their living quality. 
      This was calculated based on what percentage the country was in poverty." )
    })
  }
)
