library(dplyr)
library(ggplot2)
library(stringr)

# Data Diarrhea Death Rate
diarrheal_death <- read.csv("project-mynahshetty11/data/data_diarrhea_death_rate.csv")

# sort by total cases in every 5 years
diarrheal_death %>%
group_by(Year = Year -Year %% 5) %>%
  summarise(Number.of.deaths = sum(Number.of.deaths))
View(diarrheal_death)

plot(diarrheal_death$Year, diarrheal_death$Number.of.deaths)

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
                      
