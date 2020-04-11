#This is a script to build data to be used for covid 19 dashboard
#
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

#Load data
# Shorthand	Meaning
# ~	Home directory
# .	Current working directory
# ..	One directory up from current working directory
# ../..	Two directories up from current working directory

# myfilepath <- file.path("./data","time_series_covid19_confirmed_global.csv")
# df_confirmed_global <- read.csv(file = myfilepath, stringsAsFactors = FALSE)


#Convert country to a factor var
convertCountrytoFactorandGather <- function(df) {
  df$Country.Region <- factor(df$Country.Region)
  # Convert data from wide to long using gather from tidyr package
  # names(df)[ncol(df)] returns the name of the last column
  df_long <- gather(df, datereported,culTotal,X1.22.20:names(df)[ncol(df)], factor_key=TRUE)
  
  return(df_long)
}


filterKenya<-function(df) {
  df<-df %>%
    filter(Country.Region=="Kenya")
  return(df)
}


addNewCasesField <- function(df) {
  df$new_cases<-0
  prev <- 0
  for (row in 1:nrow(df)){
    #print(kenya[row, 'culTotal'])
    cur <- df[row, 'culTotal']
    
    #print(paste("Previous ", prev, " Current ",cur)) #uncomment to debug
    new_cases <- cur - prev
    df[row, 'new_cases'] <- new_cases
    prev <- cur
  }
  return(df)
}

# Create dates
# Function to convert string to dates
createDate <- function(df) {
  date_String <- str_split(df['datereported'],"X")[1]
  #print(mdy(date_String)) #uncomment to debug
  dateVal <- mdy(date_String)
  return(as.character(dateVal))
}

addDateReportedField <- function(df) {
  df$datereported_0 <- apply(df,1,createDate)
  return(df)
}
  


tidyUpData <- function(df) {
  df_long <- convertCountrytoFactorandGather(df)
  df_long_ke <- filterKenya(df_long)
  df_long_ke <- addNewCasesField(df_long_ke)
  df_long_ke <- addDateReportedField(df_long_ke)
  return (df_long_ke)
}

createFinalData_set <- function(df1_cases,df2_deaths,df3_recover) {
  finaldf <- df1_cases
  finaldf <- finaldf %>%
    rename(cum_total_cases = culTotal)
  
  #deaths
  df2_deaths <- df2_deaths %>%
    select(datereported,culTotal,new_cases) %>%
    rename(cum_total_deaths = culTotal, new_deaths = new_cases)
  
  #recovery
  df3_recover <- df3_recover %>%
    select(datereported,culTotal,new_cases) %>%
    rename(cum_total_recovered = culTotal, new_recovered = new_cases)
  
  # Merge db
  finaldf <- finaldf %>%
    left_join(df2_deaths, by = 'datereported')
  
  # Merge db
  finaldf <- finaldf %>%
    left_join(df3_recover, by = 'datereported')
  
  return(finaldf)
}

writeCsvFile <- function(df) {
  
  file_name<-paste("./data/data_covid_19", ".csv", sep="")
  write.csv(covid_df_all, file_name)
}



#Get the data
myfilepath <- file.path("./data","time_series_covid19_confirmed_global.csv")
myfilepathdeaths <- file.path("./data","time_series_covid19_deaths_global.csv")
myfilepathrecovered <- file.path("./data","time_series_covid19_recovered_global.csv")

df_confirmed_global <- read.csv(file = myfilepath, stringsAsFactors = FALSE)
df_deaths_global <- read.csv(file = myfilepathdeaths, stringsAsFactors = FALSE)
df_recovered_global <- read.csv(file = myfilepathrecovered, stringsAsFactors = FALSE)

#Call tidy up data functions
covid_df_c_test <- tidyUpData(df_confirmed_global)
covid_df_d_test <- tidyUpData(df_deaths_global)
covid_df_r_test <- tidyUpData(df_recovered_global)

#merge the data
covid_df_all <- createFinalData_set(covid_df_c_test, covid_df_d_test, covid_df_r_test)

# write the data to csv file
writeCsvFile(covid_df_all)
