# server.R
library(dplyr)
library(shiny)
library(plotly)

# data cleaning handwash and poverty files 

poverty <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-mynahshetty11/main/data/data_poverty.csv")
handwash <- read.csv("https://raw.githubusercontent.com/info201b-au2022/project-mynahshetty11/main/data/data_handwash.csv")

handwash_filtered <- slice(handwash,3:n())
names(handwash_filtered) <-  paste0(names(handwash_filtered), ",") %>%
  str_replace("\\.1,", ", Urban") %>%
  str_replace("\\.2,", ", Rural") %>%
  str_replace("\\,$", ", Both Rural and Urban")

#Convert handwash years to rows
handwash_filtered <- rename(handwash_filtered, Countries = "X, Both Rural and Urban") %>%
  gather("Year_and_type", "Percent", -Countries) %>%
  separate("Year_and_type", into = c("Year","Urban_Rural_or_Both"), sep = ", ")%>%
  rename(Country = Countries)

#Convert Percentages to integers
#String to integer function is from:
#https://www.tutorialkart.com/r-tutorial/r-convert-string-to-integer/#:~:text=To%20convert%20String%20to%20Integer%20in%20R%20programming%2C%20call%20strtoi,respect%20to%20the%20specified%20base.
handwash_filtered$Percent <- strtoi(handwash_filtered$Percent)
handwash_filtered <- filter(handwash_filtered, !is.na(handwash_filtered$Percent))

#Convert Year to actual years
handwash_filtered$Year <- strtoi(str_replace(handwash_filtered$Year, "X", ""))

(mean(na.omit(poverty$percPoverty)))

handwash_filtered <- slice(handwash,3:n())
names(handwash_filtered) <-  paste0(names(handwash_filtered), ",") %>%
  str_replace("\\.1,", ", Urban") %>%
  str_replace("\\.2,", ", Rural") %>%
  str_replace("\\,$", ", Both Rural and Urban")

#Convert handwash years to rows
handwash_filtered <- rename(handwash_filtered, Countries = "X, Both Rural and Urban") %>%
  gather("Year_and_type", "Percent", -Countries) %>%
  separate("Year_and_type", into = c("Year","Urban_Rural_or_Both"), sep = ", ")%>%
  rename(Country = Countries)

#Convert Percentages to integers
#String to integer function is from:
#https://www.tutorialkart.com/r-tutorial/r-convert-string-to-integer/#:~:text=To%20convert%20String%20to%20Integer%20in%20R%20programming%2C%20call%20strtoi,respect%20to%20the%20specified%20base.
handwash_filtered$Percent <- strtoi(handwash_filtered$Percent)
handwash_filtered <- filter(handwash_filtered, !is.na(handwash_filtered$Percent))

#Convert Year to actual years
handwash_filtered$Year <- strtoi(str_replace(handwash_filtered$Year, "X", ""))

(mean(na.omit(poverty$percPoverty)))

poverty_df <- data.frame(poverty$percPoverty, poverty)
poverty_df <- data.frame(poverty_df$percPoverty, poverty)
poverty_df$percPoverty <- as.double(poverty_df$percPoverty)


handwash_df <- handwash_filtered %>% mutate(no = (100 - handwash_filtered$Percent)) 
poverty.df <- poverty_df %>% mutate(
  country_type = ifelse(poverty_df$percPoverty  < 10,"rich", ifelse(poverty_df$percPoverty < 20 & poverty_df$percPoverty > 10,  
                                                                    "average", ifelse(poverty_df$percPoverty < 30 & poverty_df$percPoverty > 20, "slightly poor", ifelse(poverty_df$percPoverty < 40 & 
                                                                                                                                                                           poverty_df$percPoverty > 30, "poor", "extremely poor"))))
)

poverty.df <- poverty.df %>% 
  rename("Country" = "country")

merge <- inner_join(poverty.df, handwash_df)
merge %>%filter(!is.na(country_type))



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
