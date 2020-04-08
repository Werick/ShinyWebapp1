#UI
library(shiny)
library(shinydashboard)
library(leaflet)
ui <- dashboardPage(
  dashboardHeader(title = "COVID 19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daily Covid-19 Incidence", tabName = "first", icon = icon("dashboard")),
      menuItem("second", tabName = "second", icon = icon("th")),
      menuItem("Maps", tabName = "map", icon = icon("th")),
      menuItem("Cummulative Incidence", tabName = "epicumm", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first",
              h2("COVID-19 Epidermic Curve"),
              plotOutput("map1")
              
              ),
      tabItem(tabName = "second",
              h2("Second Tab Content"),
              plotOutput("map2")
              ),
      tabItem(tabName = "map",
              h2("Map Tab Content"),
              leafletOutput("testMap")
      ),
      tabItem(tabName = "epicumm",
              h2("Culumative Numbers"),
              plotOutput("plot3")
      )
    )
  )
)