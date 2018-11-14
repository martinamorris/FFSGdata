#' ---
#' title: "Scraping data from fatalencounters.org"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://fatalencounters.org/>.

#install.packages(c('googlesheets', 'RecordLinkage'))
library(googlesheets)
library(dplyr)

# assumes current dir is local repo dir.
# Don't need this anymore -- best not to setwd to play nice with sourcing from 
# other files. Instead use mydir and paste to save out.
#setwd(paste(mydir, 'Data/Scraping/ScrapedFiles/', sep="/"))

# fatal encounters, <https://fatalencounters.org/>

# get google sheet
# a few records have unreadable "Year" entries, but the record is read, and year can be retrieved
# from the Date of Injury field.
# note you can retrieve/view parsing problems with problems(fe)

#' Scrape the Census Bureau's website for population data
#' 
#' @param url URL for Fatal Encounters spreadsheet
#' @param save_file filename to be saved in /ScrapedFiles/
#' @return Void. Saves the data to `save_file`.
#' @export
scrape_FE_data <- function(url, save_file) {
  fe <- gs_read_csv(gs_url(url))
  
  ##################################################################################
  
  
  # remove non-pertinent columns; these include columns "read me", 
  # undocumented columns with only one data point, and a duplicate ID col
  fe <- fe[,c(-1, -28, -26, -25, -23, -22)]
  
  # Clean up column names
  colnames(fe) <- c('name', 'age', 'sex', 'race', 'URLpic', 'dateMDY', 'address',
                    'city', 'state',	'zip', 'county',	'fullAddress', 'latitude', 'longitude',
                    'agency', 'causeOfDeath', 'circumstances', 
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
  
  
  ## Clean up field entries
  
  # clean up year, using dateMDY variable -- this will fix the parsing errors
  fe.clean$year <- as.numeric(substr(fe.clean$dateMDY,7,10))
  
  # fix spelling errors in gender and race
  replacement <- function(x) {
    if (is.na(x)) {
      return(NA)
    }
    if (RecordLinkage::jarowinkler(tolower(x), "female") >= 0.9) {
      return("Female")
    } else if (RecordLinkage::jarowinkler(tolower(x), "male") >= 0.9) {
      return("Male")
    }
  }
  fe.clean$sex <- as.character(lapply(fe.clean$sex, replacement))
  
  replacement.2 <- function(x) {
    if (is.na(x)) {
      return(NA)
    }
    if (RecordLinkage::jarowinkler(x, "Hispanic/Latino") >= 0.9) {
      return("Hispanic/Latino")
    } else if (RecordLinkage::jarowinkler(x, "European-American/White") >= 0.9) {
      return("European-American/White")
    } else if (RecordLinkage::jarowinkler(x, "African-American/Black") >= 0.9) {
      return("African-American/Black")
    } else if (RecordLinkage::jarowinkler(x, "Asian/Pacific Islander") >= 0.9) {
      return("Asian/Pacific Islander")
    } else if (RecordLinkage::jarowinkler(x, "Middle Eastern") >= 0.9) {
      return("Middle Eastern")
    } else if (RecordLinkage::jarowinkler(x, "Native American/Alaskan") >= 0.9) {
      return("Native American/Alaskan")
    } else if (RecordLinkage::jarowinkler(x, "Race unspecified") >= 0.9) {
      return("Race unspecified")
    } 
  }
  fe.clean$race <- as.character(lapply(fe.clean$race, replacement.2))
  
  
  # save cleaned copy
  save.image(save_file)
}
