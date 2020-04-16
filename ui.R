#UI
library(shiny)
library(shinydashboard)
library(lubridate)
#library(gdata)
library(leaflet)


ui <- dashboardPage(
  dashboardHeader(title = "COVID 19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daily Covid-19 Incidence", tabName = "first", icon = icon("dashboard")),
      menuItem("Incidence, Recovery and Death", tabName = "second", icon = icon("th")),
      menuItem("Map By County", tabName = "map", icon = icon("th")),
      menuItem("Cummulative Incidence", tabName = "epicumm", icon = icon("th")),
      h4(paste("Updated on: ",Sys.Date()-days(1)))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first",
              h2(paste("COVID-19 Epidermic Curve with the Predicted Growth as at ", Sys.Date()-days(1))),
              fluidRow(
                tags$head(tags$style(HTML("
                                #totalCases{
                                  text-align: center;
                                  fonts-weight: bold;
                                }
                                #totalDeaths{
                                  text-align: center;
                                }
                                #totalRecovered{
                                  text-align: center;
                                }
                                #totalActive{
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                 
                                }
                                "))),
                box(title = "Total Confirmed Cases",width =  3, status = "warning", solidHeader = TRUE,
                    
                       textOutput("totalCases")),
                box(title = "Total Deaths (n,%)", width =  3, status = "danger",solidHeader = TRUE,
                       textOutput("totalDeaths")),
                box(title = "Total Recovered (n,%)", width =  3,status = "success",solidHeader = TRUE,
                       textOutput("totalRecovered")),
                box(title = "Active Cases (n,%)", width =  3,status = "warning",solidHeader = TRUE,
                    textOutput("totalActive"))
                
                # Dynamic infoBoxes
                # infoBoxOutput("totalRecovered"),
                # infoBoxOutput("approvalBox")
                
              ),
              fluidRow(
                tags$head(tags$style(HTML("
                                .box-header {
                                  text-align: center;
                                 fonts-weight: bold;
                                }
                                "))),
                box(title="Epidermic Curve with Predicted Incidence and 95% CI", solidHeader = TRUE,
                    plotOutput("map1")),
                
                box(title="Daily Incidence", solidHeader = TRUE,
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
              h2("Confirmed COVID-19 Cases By County"),
              leafletOutput("testMap")
      ),
      tabItem(tabName = "epicumm",
              h2("Covid-19 Cummulative Incidence"),
              plotOutput("plot3")
      )
    )
  )
)