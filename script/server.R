# Server
#Load the library
library(EpiCurve)
library(incidence)
library(ggplot2)
library(dplyr)

getwd()
#Get data
filepath <- "D:/R-Dev/ShinyWebapp1/data/COVID_19.csv"
myfile <- file.path(paste(getwd(),"/data"), "COVID_19.csv") 
covid_df <- read.csv(file = filepath, stringsAsFactors = FALSE)

covid_df$dateRep_0<-as.Date(as.character(covid_df$dateRep), format = "%d/%m/%Y")

covid_df<- covid_df %>%
  filter(countriesAndTerritories=="Kenya") %>%
  arrange(dateRep_0)



server <- function(input, output){
  # Map Server code
  output$testMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-93.65, 42.0285, zoom = 17)
  })
  output$map1 <- renderPlot({
    my_theme <- theme_bw(base_size = 12) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"))
    
    idata <- as.incidence(x = covid_df[,c("cases","deaths")], dates = covid_df$dateRep_0 )
    plot(idata, border="white") + my_theme + theme(legend.position = c(0.15, 0.8))
  })
  
  output$map2 <- renderPlot({
    EpiCurve(covid_df,date = "dateRep_0", period = "day", freq = "cases",
             ylabel="Number of cases",
             title = "Kenya COVID-19 Epidemic Curve")
  })
  
  output$plot3 <- renderPlot({
    idata <- as.incidence(x = covid_df[,c("cases","deaths")], dates = covid_df$dateRep_0 )
    iculum <- cumulate(idata)
    plot(iculum)
    
  })
}
