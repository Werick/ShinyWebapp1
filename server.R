# Server
#Load the library
library(EpiCurve)
library(incidence)
library(ggplot2)
library(dplyr)
library(DT)

#source("script/helperFunctions.R")

#Get data

#Load data
# Shorthand	Meaning
# ~	Home directory
# .	Current working directory
# ..	One directory up from current working directory
# ../..	Two directories up from current working directory


myfilepath <- file.path("data","data_covid_19.csv")


covid_df_all <- read.csv(file = myfilepath, stringsAsFactors = FALSE)



covid_df_all$datereported_0<-as.Date(as.character(covid_df_all$datereported_0))

covid_df_all<- covid_df_all %>%
  arrange(datereported_0)



server <- function(input, output){
  # Map Server code
  output$testMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 0.1769, lng=37.9083, zoom = 6)
  })
  
  my_theme <- theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"))
  
  output$totalCases<- renderText({
    sum(covid_df_all$new_cases)
  })
  
  output$totalDeaths<- renderText({
    sum(covid_df_all$new_deaths)
  })
  
  output$totalRecovered <- renderText({
    sum(covid_df_all$new_recovered)
    
  })
  
  output$map1 <- renderPlot({
    
    idata <- as.incidence(x = covid_df_all[,c("new_cases")], dates = covid_df_all$datereported_0 )
    
    # Modelling Incidence. This done using the fit function from the incidence package
    early.fit <- fit(idata)
    plot(idata, fit = early.fit, border="white") + my_theme + theme(legend.position = c(0.15, 0.8))
  })
  
  output$plot2 <- renderPlot({
    idata <- as.incidence(x = covid_df_all[,c("new_cases","new_deaths","new_recovered")], dates = covid_df_all$datereported_0 )
    
    plot(idata, border="white") + my_theme + theme(legend.position = c(0.15, 0.8))
  })
  
  output$plot3 <- renderPlot({
    idata <- as.incidence(x = covid_df_all[,c("new_cases","new_deaths","new_recovered")], dates = covid_df_all$datereported_0 )
    iculum <- cumulate(idata)
    plot(iculum)
    
  })
  
  #Dislay data table
  output$dailyIncidence <-  DT::renderDataTable({
    dt <- covid_df_all %>%
      select(datereported_0,new_cases,new_deaths) %>%
      filter(new_cases>0 | new_deaths >0)
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download1'));"),
              extensions = 'Buttons')
    
  })
  
  #Method to handle data download
  output$download1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(covid_df_all, file)
    }
  )
}
