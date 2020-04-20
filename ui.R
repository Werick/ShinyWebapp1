#UI
library(shiny)
library(shinydashboard)
library(lubridate)
#library(gdata)
library(leaflet)
library(plotly)


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
              h2(paste("KENYA COVID-19 STATISTICS AS AT ", Sys.Date()-days(1))),
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
                    plotlyOutput("plot1_epi_curve")),
                
                box(title="Daily Incidence", solidHeader = TRUE,
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('dailyIncidence')),
                    downloadButton("download1","Download csv")
                    )
              )
              ),
      tabItem(tabName = "second",
              fluidRow(
                
                box(title=paste("KENYA COVID-19 Combined Daily Incidence, Recovery and Death as at",Sys.Date()-days(1)), solidHeader = TRUE,
                    width = 12,                    
                    plotlyOutput("plot2_combined") 
                )
                # ,
                # tabBox(
                #   title = "Daily COVID-19 Recovered",
                #   # The id lets us use input$tabset1 on the server to find the current tab
                #   id = "tabset1", height = "200px",
                #   tabPanel("linear", plotlyOutput("plot_rec_cases_linear")),
                #   tabPanel("logarithmic", plotlyOutput("plot_rec_cases_log"))
                # )
              )
              # ,
              # fluidRow(
              #   tabBox(
              #     title = "Cummulative Cases",
              #     # The id lets us use input$tabset1 on the server to find the current tab
              #     id = "tabset1", height = "200px",
              #     tabPanel("linear", plotlyOutput("plot_cases_linear")),
              #     tabPanel("logarithmic", plotlyOutput("plot_cases_log"))
              #   ),
              #   tabBox(
              #     title = "Cummulative Recovered",
              #     # The id lets us use input$tabset1 on the server to find the current tab
              #     id = "tabset2", height = "200px",
              #     tabPanel("linear", plotlyOutput("plot_recovered_linear")),
              #     tabPanel("logarithmic", plotlyOutput("plot_recovered_log"))
              #   )
              # )
              ),
      tabItem(tabName = "map",
              h2(paste("Confirmed COVID-19 Cases By County in KENYA as at", Sys.Date()-days(1))),
              leafletOutput("testMap")
      ),
      tabItem(tabName = "epicumm",
              fluidRow(
                box(title=paste("KENYA Covid-19 Cummulative Incidence, Deaths and Recovery as at", Sys.Date()-days(1)), solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("plot3_cummulative")
                )
              )
              ,
              fluidRow(

                tabBox(
                  width = 12,
                  title = "Cummulative COVID-19 Cases",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "100px",
                  tabPanel("linear", plotlyOutput("plot_cases_linear")),
                  tabPanel("logarithmic", plotlyOutput("plot_cases_log"))
                )
              )
      )
    )
  )
)