#Sean Lee Chart 3
library(dplyr)
library(ggplot2)
library(stringr)
#Cholera data
cholera <- read.csv("project-mynahshetty11/data/data_cholera_infection_rate.csv")

#code to sort by total amount of cases every 5 years. Plot might have too big numbers
cholera %>%
group_by(Year = Year - Year %% 5) %>% 
  summarise(Number.of.reported.cases.of.cholera = sum(Number.of.reported.cases.of.cholera))
View(cholera)

plot(cholera$Year, cholera$Number.of.reported.cases.of.cholera)
#Handwashing data
handwash <- read.csv("project-mynahshetty11/data/data_handwash.csv")

#Rename columns for handwash
handwash_recent <- slice(handwash,3:n())
names(handwash_recent) <-  paste0(names(handwash_recent), ",") %>%
                               str_replace("\\.1,", ", Urban") %>%
                          str_replace("\\.2,", ", Rural") %>%
                          str_replace("\\,$", ", Both")

#Convert to rows
handwash_recent <- rename(handwash_recent, Countries = "X, Both") %>%
                      gather("Year_and_type", "Percent", -Countries) %>%
                    separate("Year_and_type", into = c("Year","Urban,Rural, or Both"), sep = ", ")
                      
