# Server
#Load the library
library(shinydashboard)
library(EpiCurve)
library(incidence)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(sf) # R Geospatial  data abstraction library
#library(rgdal) # R Geospatial  data abstraction library
#library(gdata)
library(leaflet)

#source("script/helperFunctions.R")

#Get data

#Load data
# Shorthand	Meaning
# ~	Home directory
# .	Current working directory
# ..	One directory up from current working directory
# ../..	Two directories up from current working directory

# sp_df Means Spatial polygon dataframe
# This method reads faster tan readOGR
mysp_df_county_polygons <- read_sf(dsn=path.expand("data/kenyan-counties"), layer = "County") 



myfilepath <- file.path("data","data_covid_19.csv")
file_path_county <- file.path("data","covid19_kenya_confirmed_county.csv")


covid_df_all <- read.csv(file = myfilepath, stringsAsFactors = FALSE)
covid_county_df <- read.csv(file = file_path_county, stringsAsFactors = FALSE)


# add covid 19 stats to sp_df
add_covid19_stats <- function(df) {
  county <- toupper(df['COUNTY'])
  
  if (county == "KEIYO-MARAKWET") {
    county<-toupper('Elgeyo-Marakwet')
  } 
  else if (county == 'THARAKA') {
    county<-toupper('Tharaka-Nithi')
  }
  
  county_df <- covid_county_df[toupper(covid_county_df$admin1Name_en)==county,]
  #print(paste(county))
  if (nrow(county_df>0)) {
    return(county_df$covid_19)
  }
  else {
    return(0)
  }
}

add_county_code <- function(df) {
  county <- toupper(df['COUNTY'])
  
  if (county == "KEIYO-MARAKWET") {
    county<-toupper('Elgeyo-Marakwet')
  } 
  else if (county == 'THARAKA') {
    county<-toupper('Tharaka-Nithi')
  }
  
  county_df <- covid_county_df[toupper(covid_county_df$admin1Name_en)==county,]
  #print(paste(county))
  if (nrow(county_df>0)) {
    return(county_df$admin1Pcode)
  }
  else {
    return(0)
  }
  
}

mysp_df_county_polygons$Covid_19_stats <- apply(mysp_df_county_polygons,1,add_covid19_stats)
mysp_df_county_polygons$county_code <- apply(mysp_df_county_polygons,1,add_county_code)

covid_df_all$datereported_0<-as.Date(as.character(covid_df_all$datereported_0))

covid_df_all<- covid_df_all %>%
  arrange(datereported_0)

#Add cololrs to the map based on COVID 19 stataus
bins <- c(0, 2, 5, 10, 20, 40, 80, 150,Inf)
pal <- colorBin("YlOrRd", domain = mysp_df_county_polygons$Covid_19_stats, bins = bins)

#Get the longitude and Latitude from sp data
#THis worked using readOGR not sure if it will still work
lng_lan <-st_centroid(mysp_df_county_polygons)


# add the longitude and Latitude from the above matrix
#uncomment these  lines if you want to add markers
county_coords <- do.call(rbind, st_geometry(lng_lan)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

mysp_df_county_polygons$lng <- county_coords$lon # col 1
mysp_df_county_polygons$lat <- county_coords$lat # col 2

#get counties with reported cases only
 county_with_cases <- mysp_df_county_polygons %>%
   filter(Covid_19_stats>0)


labels <- sprintf(
  "<strong>%s County(%s)</strong><br/>Confirmed Cases: %g ",
  mysp_df_county_polygons$COUNTY, mysp_df_county_polygons$county_code, mysp_df_county_polygons$Covid_19_stats) %>% 
  lapply(htmltools::HTML)

server <- function(input, output){
  # Map Server code
  output$testMap <- renderLeaflet({
    leaflet(data = mysp_df_county_polygons) %>%
      addTiles() %>%
      setView(lat = 0.1769, lng=37.9083, zoom = 6)%>%
      addPolygons(fillColor = ~pal(Covid_19_stats), 
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 5, color = "red", fillOpacity = 0.7, bringToFront = TRUE), 
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))%>% 
      addLegend(pal = pal, values = ~Covid_19_stats, opacity = 0.7, 
                title = NULL, position = "bottomright") %>%
      addCircleMarkers(
        lng = ~county_with_cases$lng, lat = ~county_with_cases$lat,
        radius = 3,
        color = "black",
        stroke = FALSE, fillOpacity = 0.5
      )
      #addMarkers(lng = ~county_with_cases$lng, lat = ~county_with_cases$lat,popup =~county_with_cases$COUNTY)
    
  })
  
  #Use reactive values to make faster retrieval
  t_cases <- reactive({sum(covid_df_all$new_cases)})
  t_deaths <- reactive({sum(covid_df_all$new_deaths)})
  t_recovered <- reactive({sum(covid_df_all$new_recovered)})
  
  my_theme <- theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"))
  
  output$totalCases<- renderText({
    t_cases()
  })
  
  output$totalDeaths<- renderText({
    d <- t_deaths()
    p <- trunc(d/t_cases()*100)
    paste(d,"(",p,"%)")
  })
  
  output$totalRecovered <- renderText({
    r <- t_recovered()
    p <- trunc(r/t_cases() *100)
    paste(r,"(",p,"%)")
  })
  
  output$totalActive <- renderText({
    a <- t_cases() - t_recovered() - t_deaths()
    p <- trunc(a/t_cases() *100) 
    paste(a,"(",p,"%)")
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
              extensions = 'Buttons',
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = 2:3))) )
    
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
