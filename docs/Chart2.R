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
