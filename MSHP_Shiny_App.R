library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data
crash_data <- read_excel("mshp_crashes.xlsx") %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE),
    Hour = hour(parse_date_time(Time, orders = c("I:M p", "H:M"))),
    Age = as.numeric(Age)
  )

#The Data only contains crash Information for the month of April 2025

# UI
ui <- fluidPage(
  titlePanel("Missouri State Highway Patrol - Crash Report April 2025"),
  tabsetPanel(
    tabPanel("Crashes Over Time", 
             dateRangeInput("dateRange", "Select Date Range:",
                            start = min(crash_data$Date), end = max(crash_data$Date)),
             plotOutput("plot_crashes_over_time")),
    
    tabPanel("Crash Count by County",
             selectInput("county", "Select County:", choices = unique(crash_data$`Crash County`), selected = "ST. CHARLES"),
             dateRangeInput("countyDateRange", "Select Date Range:",
                            start = min(crash_data$Date), end = max(crash_data$Date)),
             plotOutput("plot_county_crashes")),
    
    tabPanel("Crash Severity",
             selectInput("severity", "Select Injury Type:", choices = unique(crash_data$`Personal Injury`)),
             dateRangeInput("severityDateRange", "Select Date Range:",
                            start = min(crash_data$Date), end = max(crash_data$Date)),
             plotOutput("plot_injury_distribution")),
    
    tabPanel("Age Distribution",
             sliderInput("ageRange", "Select Age Range:", min = 0, max = 100, value = c(0, 100)),
             plotOutput("plot_age_distribution")),
    
    tabPanel("Time of Day",
             sliderInput("hourRange", "Select Hour Range:", min = 0, max = 23, value = c(0, 23)),
             plotOutput("plot_time_of_day")),
    
    tabPanel("Safety Devices",
             selectInput("safetyDevice", "Select Safety Device:", choices = unique(crash_data$`Safety Device`)),
             plotOutput("plot_safety_device")),
    
    tabPanel("Person Type",
             selectInput("personType", "Select Person Type:", choices = unique(crash_data$`Person Type`)),
             plotOutput("plot_person_type")),
    
    tabPanel("Troop Regions",
             selectInput("troop", "Select Troop:", choices = unique(crash_data$Troop)),
             plotOutput("plot_troop_crashes")),
    
    tabPanel("Top Crash Locations",
             numericInput("topN", "Number of Top Locations:", min = 1, max = 50, value = 10),
             plotOutput("plot_top_locations")),
    
    tabPanel("Injury by Age Group",
             checkboxGroupInput("injuryTypes", "Select Injury Types:", choices = unique(crash_data$`Personal Injury`), selected = unique(crash_data$`Personal Injury`)),
             plotOutput("plot_injury_by_age"))
  )
)

# Server
server <- function(input, output) {
  
  output$plot_crashes_over_time <- renderPlot({
    crash_data %>% 
      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
      count(Date) %>%
      ggplot(aes(x = Date, y = n)) +
      geom_line(group=1) +
      labs(x="Date", y="Number of Crashes", title="Daily Crashes Over Time")
  })
  
  output$plot_county_crashes <- renderPlot({
    crash_data %>% 
      filter(`Crash County` == input$county,
             Date >= input$countyDateRange[1], Date <= input$countyDateRange[2]) %>%
      count(Date) %>%
      ggplot(aes(x = Date, y = n)) +
      geom_col() +
      labs(title = paste("Crashes in", input$county), y="Crashes", x="Date")
  })
  
  output$plot_injury_distribution <- renderPlot({
    crash_data %>%
      filter(`Personal Injury` == input$severity,
             Date >= input$severityDateRange[1], Date <= input$severityDateRange[2]) %>%
      ggplot(aes(x = `Personal Injury`, fill = `Personal Injury`)) +
      geom_histogram(stat = "count") +
      labs(title = "Histogram of Injury Severity")
  })
  
  output$plot_age_distribution <- renderPlot({
    crash_data %>%
      filter(!is.na(Age), Age >= input$ageRange[1], Age <= input$ageRange[2]) %>%
      ggplot(aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "steelblue") +
      labs(title = "Age Distribution of Individuals in Crashes")
  })
  
  output$plot_time_of_day <- renderPlot({
    crash_data %>%
      filter(!is.na(Hour), Hour >= input$hourRange[1], Hour <= input$hourRange[2]) %>%
      count(Hour) %>%
      ggplot(aes(x = Hour, y = n)) +
      geom_col(fill = "tomato") +
      labs(title = "Crashes by Hour of Day")
  })
  
  output$plot_safety_device <- renderPlot({
    crash_data %>%
      filter(`Safety Device` == input$safetyDevice) %>%
      count(`Safety Device`, `Personal Injury`) %>%
      ggplot(aes(x = `Safety Device`, y = n, fill = `Personal Injury`)) +
      geom_col(position = "fill") +
      labs(title = "Safety Device Usage vs Injury Outcome", y="Proportion")
  })
  
  output$plot_person_type <- renderPlot({
    crash_data %>%
      filter(`Person Type` == input$personType) %>%
      count(`Person Type`) %>%
      ggplot(aes(x = `Person Type`, y = n, fill = `Person Type`)) +
      geom_col() +
      labs(title = "Crash Involvement by Person Type")
  })
  
  output$plot_troop_crashes <- renderPlot({
    crash_data %>%
      filter(Troop == input$troop) %>%
      count(Troop) %>%
      ggplot(aes(x = Troop, y = n, fill = Troop)) +
      geom_col() +
      labs(title = paste("Crash Count for Troop", input$troop))
  })
  
  output$plot_top_locations <- renderPlot({
    crash_data %>%
      count(`Crash Location`, sort=TRUE) %>%
      top_n(input$topN, n) %>%
      ggplot(aes(x = reorder(`Crash Location`, n), y = n)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(title = paste("Top", input$topN, "Crash Locations"), x="Location")
  })
  
  output$plot_injury_by_age <- renderPlot({
    crash_data %>%
      filter(!is.na(Age), Age <= 100, `Personal Injury` %in% input$injuryTypes) %>%
      mutate(AgeGroup = cut(Age, breaks = c(0, 18, 30, 50, 70, 100), labels = c("<18","18-30","31-50","51-70","71+"))) %>%
      count(AgeGroup, `Personal Injury`) %>%
      ggplot(aes(x = AgeGroup, y = n, fill = `Personal Injury`)) +
      geom_col(position = "dodge") +
      labs(title = "Injury Type by Age Group")
  })
}

shinyApp(ui = ui, server = server)
