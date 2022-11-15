library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

poverty <- read.csv("data/data_poverty.csv")
handwash <- read.csv("data/data_handwash.csv")

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

poverty_df <- data.frame(poverty_df$percPoverty, poverty)
poverty_df$percPoverty <- as.double(poverty_df$percPoverty)


handwash_df <- handwash_filtered %>% mutate(no = (100 - handwash_filtered$Percent)) 
View(handwash_df)

poverty.df <- poverty_df %>% mutate(
country_type = ifelse(poverty_df$percPoverty  < 10,"rich", ifelse(poverty_df$percPoverty < 20 & poverty_df$percPoverty > 10,  
    "average", ifelse(poverty_df$percPoverty < 30 & poverty_df$percPoverty > 20, "slightly poor", ifelse(poverty_df$percPoverty < 40 & 
    poverty_df$percPoverty > 30, "poor", "extremely poor"))))
 )

poverty.df <- poverty.df %>% 
  rename("Country" = "country")

merge <- inner_join(poverty.df, handwash_df)

View(merge)
View(poverty.df)

merge <- merge %>%group_by(country_type) %>%
summarise(Percent = mean(Percent))
merge <- arrange(merge, Percent)
View(merge)

merge %>%filter(!is.na(country_type))

p<-ggplot(data= na.omit(merge), aes(x=country_type, y=Percent)) +
  geom_bar(stat="identity", fill = " dark green") +
 
  xlab("Country Condition based on Poverty Rates") +
  ylab("Percentage of the Country that has Access to Handwashing") +
   scale_y_sqrt(labels = scales::comma)  +
  labs(title = "Percent of Handwashing Availibility in types of Countries", 
       subtitle = "Relationship between the average handwashing percentage and country types from 2000 to 2020", 
       caption = "This depicts the average proportion of handwashing in countries based on their classification of their living quality. 
       This was calculated based on what percentage the country was in poverty." )
p


