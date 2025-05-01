library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

covid_data <- read_csv("owid-covid-data.csv")

ui <- fluidPage(
  titlePanel("COVID-19 Visualizations by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country:",
                  choices = unique(covid_data$location))
    ),
    mainPanel(
      plotOutput("casesPlot"),
      plotOutput("deathsPlot"),
      plotOutput("vaccinationPlot"),
      plotOutput("positivityPlot"),
      plotOutput("hospitalPlot")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    covid_data %>%
      filter(location == input$country) %>%
      arrange(date)
  })

  output$casesPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = total_cases)) +
      geom_line(color = "steelblue") +
      labs(title = "Total COVID-19 Cases Over Time",
           x = "Date", y = "Total Cases")
  })

  output$deathsPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = total_deaths)) +
      geom_line(color = "firebrick") +
      labs(title = "Total COVID-19 Deaths Over Time",
           x = "Date", y = "Total Deaths")
  })

  output$vaccinationPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = people_vaccinated)) +
      geom_line(color = "forestgreen") +
      labs(title = "People Vaccinated Over Time",
           x = "Date", y = "People Vaccinated")
  })

  output$positivityPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = positive_rate)) +
      geom_line(color = "darkorange") +
      labs(title = "Test Positivity Rate Over Time",
           x = "Date", y = "Positive Rate")
  })

  output$hospitalPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = hosp_patients)) +
      geom_line(color = "purple") +
      labs(title = "Hospitalized Patients Over Time",
           x = "Date", y = "Hospitalized Patients")
  })
}

shinyApp(ui = ui, server = server)
