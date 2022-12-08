library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(shiny)
library(stringr)
library(plotly)

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
merge1 <- na.omit(merge)




## ui 

## layout


ui <- navbarPage(
  "Project",
  page_one <- tabPanel(
    "Introduction",
    titlePanel("Introduction"),
    img(src = 'world.gif', align = "left"),
    mainPanel(
      h2("Project Overview"),
      p("Our group decided to research the correlation between access to clean sanitation methods, and diagnosis of disease I countries with different levels of poverty. We decided on this topic because we felt that it is important to address how poor sanitation services can greatly affect people’s health in countries that don’t have the money to afford these services. So, as a group, we wanted to ask questions to be able to find the root of sanitation issues so that we can be a step closer to finding potential solutions.  We chose to use variables such as proportion of people without access to clean water in each country, proportion of people infected with cholera by country, and poverty rate. We used these variables because we hypothesized that countries with higher poverty rates would have less access to clean sanitation services, and therefore higher rates of cholera infection. "),
      img(src = 'image1.jpeg', align = "left"),
      p("This image is of a woman carrying jerrycans of water from a municipal water pipe of an illegal slum dwelling area in the Indian state of Uttar Pradesh, where the water and sanitation services are lacking."),
      
      h2("Major Questions"),
      p("1. What is the relationship between proportion of people without access to clean water, sanitation services and the proportion of people who get infected with cholera?"),
      p("2. What is the relationship between proportion of people with access to clean water, sanitation services and countries that have different rates of poverty?"), 
      
      
      h2("Data We Used"),
      h3("Handwash"),
      p("The handwashing data that we used gives us the proportion of people in each country that has access to cleas sanitation services.
        We were able to use this dataset in all three of our questions because they all require data about sanitation service availability by country"),
      p("Link : https://apps.who.int/gho/data/node.main.WSHHYGIENE?lang=en"),
      
      h3("Cholera") ,
      p(" The Cholera dataset was used to help answer question one. It helped us access the correlation between access to clean sanitation services, and cholera infection cases by country."),
      p("Link : https://apps.who.int/gho/data/node.main.175?lang=en"),
      
      h3("Poverty Rates"),
      p("This dataset was used to divide countries in to groups based off of their poverty rates.
        We used this data in question two to research the correlation between poverty rates of each country, and its effects on access to clean sanitation services"),
      p("Link: https://worldpopulationreview.com/country-rankings/poverty-rate-by-country"),
      
      h3("Cholera Proportions"),
      p("This dataset was used to calculate the proportions that are used in research question 1. We used proportions to account for differences in populaton sizes. So, we found the number of hcolera cases
        relative to population size. This is because, if we used total number of cholera cases, bigger countries will likely have a higher amount of cholera cases."),
      p("Link : https://data.worldbank.org/indicator/SP.POP.TOTL")
    )),
  page_two <- tabPanel(
    "Interactive Page 1: Handwashing Increase Question",
    h3("Answers the Question: "),
    h4("Has access to handwashing improved over time?"),
    sidebarLayout(
      sidebarPanel(
        selectInput("Country", "Choose a Country:",
                    choices = unique(merge1$Country)),
        
      ),
      mainPanel(
        h3("Purpose of this chart"),
        p("This chart answers the research question looking to answer the quesiton if handwashing rates have increased over time. In order to calculate this, I decided to split this data between different countries between 2000 and 2020. 
          This is an important statistic because a lack of access to handwashing is linked to the spread of deadly diseases. Based on the data, handwashing rates generally stagnated or
          increased slightly for most countries. The handwashing rates for the countries in the data set ranged from 6 percent, to 99 percent in 2022, showing the vast disparity in 
          between nations in terms of access to reliable handwashing access. "),
        plotlyOutput("plot"),
        br(),
        div(tags$em(strong("Caption:"), "This chart represents the value of handwashing of each country for each year")
        )
      )
    )),
  page_three <- tabPanel(
    "Interactive Page 2: Poverty Research Question",
    h3("Answers the Question: "),
    h4("What is the relationship between proportion of people with access to clean water, sanitation services and countries that have different rates of poverty?"),
    select_values = colnames(merge),
    sidebar_content <- sidebarPanel(
      selectInput(
        "Year",
        label = "Year",
        choices = merge$Year,
        selected = "percPoverty"
      ), plotOutput(outputId = "chart")),
    mainPanel(  h3("Purpose of the chart"),
                p("This chart answers the research question in looking at trends amongst different types of countries dealing with poverty. It is important to note how every year, the data and handwashing
                  access fluctuates. Overall, rich countries had the most access to handwashing. However, an interesting note is that in 2008, there the most handwashing
                  access was countries with 'average' rates of poverty. The most consistent was extremely poor and poor countries in the bottom percentages of handwashing access. 
                  In 2020, handwashing access for rich country was above 80%, compared to extremely poor country being below 25% in access. This raises alarms in
                  the huge wealth gap. Handwashing access should be a human right, and should be prioritized when helping countries and people in need.")
    )),    
  page_four <- tabPanel( "Summary Takeaways",
                         #img
                         h1("Summary"),
                         p(""),
                         h3("Take Away 1: "),
                         p(""),
                         h3("Take Away 2: "),
                         p(""),
                         h3("Take Away 3: "),
                         p("")
  ),
  page_five <- tabPanel(
    "Report Page"
  )
)
