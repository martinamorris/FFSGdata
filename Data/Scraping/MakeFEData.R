#' ---
#' title: "Scraping data from fatalencounters.org"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://fatalencounters.org/>.

library(googlesheets)

setwd('./ScrapedFiles/')

# fatal encounters, <https://fatalencounters.org/>

# get google sheet
url <- 'https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/export?format=csv&id=1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE&gid=0'
fe <- gs_read_csv(gs_url(url))

# View
glimpse(fe)

# Save original scraped copy
save.image('./fe.Rdata')

##################################################################################

#fe <- read.csv(file = "FE_raw.csv")

# remove non-pertinent columns; these include columns "read me", undocumented columns with only one
# data point, and a duplicate ID col
fe <- fe[,c(-1, -28, -26, -25, -23, -22)]

# Clean up column names
colnames(fe) <- c('name', 'age', 'sex', 'race', 'URLpic', 'dateDMY', 'address',
                       'city', 'state',	'zip', 'county',	'fullAddress', 'latitude', 'longitude','agency', 'causeOfDeath', 'circumstances', 
                       'officialDisposition', 'URLarticle', 'mentalIllness',
                       'Description','year')

# remove data points that are not fact-checked
boundary <- which(fe$name == "Items below this row have not been fact-checked.")
fe <- filter(fe, row_number() < boundary)

dim(fe)

# ID non-police-shootings
suicidal.causes <- c("Drug overdose",
                     "Murder-suicide", 
                     "Murder/suicide", 
                     "Ruled an overdose", 
                     "Ruled natural causes", 
                     "Ruled suicide", 
                     "Substance use", 
                     "suicide")
fe$kbp.filter <- ifelse(toupper(fe$`officialDisposition`) 
                             %in% toupper(suicidal.causes), 
                             "killed by self", "killed by police")

# Filter out non-police-shootings and write out clean FE csv
fe.clean <- filter(fe, kbp.filter == "killed by police")

# save cleaned copy
save.image('./fe.clean.Rdata')
