#' ---
#' title: "Scraping data from fatalencounters.org"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://fatalencounters.org/>.

library(googlesheets)

setwd('./ScrapedFiles/')

# fatal encounters, <https://fatalencounters.org/>
FEScraper <- function() {
  # get google sheet
  url <- 'https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/export?format=csv&id=1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE&gid=0'
  data <- gs_read_csv(gs_url(url))
  
  # write
  write.csv(data, "FE_raw.csv")
  
  # View
  glimpse(data)
}

##################################################################################

FECleaner <- function() {
  data <- read.csv(file = "FE_raw.csv")
  
  # remove non pertinant columns; these include columns "read me", columns with only one
  # data point, and a duplicate ID col
  data <- data[,c(-2, -29, -27, -26, -24, -23, -22)]
  
  # Clean up column names
  colnames(data) <- c('ID', 'name', 'age', 'sex', 'race', 'URLpic', 'dateDMY', 'address',
                         'city', 'state',	'zip', 'county',	'agency', 'causeOfDeath', 'circumstances', 
                         'officialDisposition', 'URLarticle', 'mentalIllness', 'submitted', 'status',
                         'Description','year')
  
  dim(data)
  
  # ID non-police-shootings
  suicidal.causes <- c("Drug overdose",
                       "Murder-suicide", 
                       "Murder/suicide", 
                       "Ruled an overdose", 
                       "Ruled natural causes", 
                       "Ruled suicide", 
                       "Substance use", 
                       "suicide")
  data$kbp.filter <- ifelse(toupper(data$`officialDisposition`) 
                               %in% toupper(suicidal.causes), 
                               "killed by self", "killed by police")
  
  # Filter out non-police-shootings and write out clean FE csv
  fe.clean <- filter(data, kbp.filter == "killed by police")
  
  write.csv(fe.clean, file="FE.csv")
}

###################################################################################

FEScraper()
FECleaner()