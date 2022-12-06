ui <- navbarPage(
  "Project",
  page_one <- tabPanel(
    "Introduction",
    titlePanel("Introduction"),
    #img
    mainPanel(
      h2("Project Overview"),
      p("In "),
      h2("Major Questions"),
      p("1. What is the relationship between proportion of people without access to clean water, sanitation services and the proportion of people who get infected with cholera?,
         2. What is the relationship between proportion of people with access to clean water, sanitation services and countries that have different rates of poverty.
         3. Has access to handwashing services increased over time?"),
      h2("Data We Used"),
      h3("Handwash"),
      p("The handwashing data that we used gives us the proportion of people in each country that has access to cleas sanitation services.
        We were able to use this dataset in all three of our questions because they all require data about sanitation service availability by country"),
      h3("Cholera") ,
      p("We used the Cholera dataset in question one specifically because in this question we delved deeper in to the relationship between accessibility to sanitation services
        and cholera cases."),
      h3("Poverty Rates") ,
      p("This dataset was used to divide countries in to groups based off of their poverty rates.
        We used this data in question two to research the correlation between poverty rates of each country, and its effects on access to clean sanitation services"),
      h3("Proportions of Cholera") ,
      p("This dataset was used to calculate the proportions that are used in research question 1. We used proportions to account for differences in populaton sizes. So, we found the number of hcolera cases
        relative to population size. This is because, if we used total number of cholera cases, bigger countries will likely have a higher amount of cholera cases.")
    )
      ),
  page_two <- tabPanel(
    "Interactive Page 1"
  ),
  page_three <- tabPanel(
    "Interactive Page 2"
  ),
  page_four <- tabPanel(
    "Summary Takeaways",
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
