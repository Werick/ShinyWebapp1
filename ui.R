#UI
library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "COVID 19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daily Covid-19 Incidence", tabName = "first", icon = icon("dashboard")),
      menuItem("Incidence, Recovery and Death", tabName = "second", icon = icon("th")),
      menuItem("Map By County", tabName = "map", icon = icon("th")),
      menuItem("Cummulative Incidence", tabName = "epicumm", icon = icon("th")),
      h4("Last Update Date: ")
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first",
              h2(paste("COVID-19 Epidermic Curve with the Predicted Growth as at ", Sys.Date()-days(1))),
              fluidRow(
                box("Total Confirmed cases",width =  4,
                       textOutput("totalCases")),
                box("Total Deaths", width =  4,
                       textOutput("totalDeaths")),
                box("Total Recovered", width =  4,
                       textOutput("totalRecovered"))
                
              ),
              fluidRow(
                box("EPI Curve", solidHeader = TRUE,
                    plotOutput("map1")),
                
                box("Daily Incidence", solidHeader = TRUE,
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('dailyIncidence')),
                    downloadButton("download1","Download csv")
                    )
              )
              ),
      tabItem(tabName = "second",
              h2("Incidence, Recovery and Death"),
              plotOutput("plot2")
              ),
      tabItem(tabName = "map",
              h2("Distribution of COVID-19 By County"),
              leafletOutput("testMap")
      ),
      tabItem(tabName = "epicumm",
              h2("Covid-19 Cummulative Incidence"),
              plotOutput("plot3")
      )
    )
  )
)