library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(stringr)


# total handwashing with soap

total_handwashing <- data_6_ %>%
  select('...1', `2020...2`, `2019...5`, `2018...8`, `2017...11`, `2016...14`, `2015...17`, `2014...20`, `2013...23`, `2012...26`, `2011...29`, `2010...32`, `2009...35`, `2008...38`, `2007...41`, `2006...44`, `2005...47`, `2004...50`, `2003...53`, `2002...56`, `2001...59`)
total_handwashing<- rename(total_handwashing, countries = '...1')
total_handwashing <- rename( total_handwashing, '2020' = '2020...2', '2019' = `2019...5`,  '2018' = `2018...8`, '2017' = `2017...11`,  '2016' = `2016...14`, '2015' = `2015...17`, '2014' = `2014...20`, '2013' = `2013...23`, '2012' = `2012...26`, '2011' = `2011...29`, '2010' = `2010...32`, '2009' = `2009...35`, '2008' = `2008...38`, '2007' = `2007...41`, '2006' = `2006...44`, '2005' = `2005...47`, '2004' = `2004...50`, '2003' = `2003...53`, '2002' = `2002...56`, '2001' = `2001...59` )
View(total_handwashing)

total_handwashing = total_handwashing[-1, ]

total_handwashing <- total_handwashing %>%
  pivot_longer(-c('countries'), names_to = "date", values_to = "total_percentage")
View(total_handwashing)

total_handwashing = total_handwashing[-1:-20, ]

total_handwashing <- na.omit(total_handwashing)
View(total_handwashing)

total_handwashing$total_percentage <- strtoi(total_handwashing$total_percentage)
total_handwashing$date <- strtoi(total_handwashing$date)

total_handwashing <- total_handwashing %>%
  group_by(countries, date) %>%
  summarise(total_percentage = sum(total_percentage)) %>%
  ungroup()

total_handwashing <- total_handwashing %>%
  group_by(date) %>%
  summarise(total_percentage = mean(total_percentage))


total_handwashing_plot <- total_handwashing %>%
  ggplot(aes(x = date, y = total_percentage)) + 
  geom_line(alpha = .4, color = "seagreen") + 
  xlab("Date") +
  ylab("Percentage of People with Access to Handwashing Services") +
  scale_y_sqrt(labels = scales::comma) + 
  labs(title = "Average total percentage of Handwashing Access per Year",
       subtitle = "Relationship between Handwashing Service Aaccessibility and Date",
       caption = " This chart was created to determine the correlation between handwashing access and time.
                  This will help people visualize how handwashing access has developed over the past years.
                  We initially thought that the graph would show very visible positive growth, however,
                  we can now see how their are quite a few dips in the chart that we did not expect.It is observed
                  how the chart does seem to be levelling out in the recent years. This answers our 
                  question #2 of if access to handwashing services has increased over time.")

