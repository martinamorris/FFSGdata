#' ---
#' title: "Module for scraping data from an arbitrary URL"
#' author: "Vaughn Johnson"
#' date: 2018-10-24
#' ---

library(data.table)
library(xlsx)
library(dplyr)


scrape_url <- function(url) {
  # Extracts data from a url, adds it to
  # central directory of scraped files
  
  # Temporary directory
  TMP = ".tmp"
  
  # List of possible excel extensions
  XLS_EXT = c("xlsx", 'xls')
  
  # List of possible csv extensions
  CSV_EXT = c('csv', 'txt')
  
  file_type = file_ext(url)
  
  # Save File
  download.file(url, destfile = TMP, mode = 'wb')

  if (file_type %in% XLS_EXT) {
    violence_data <- read_xlsx(TMP)
    
  } 
  
  else if (file_type %in% CSV_EXT) {
    violence_data <- read.csv(TMP)
  } 
  
  else {
    print("Unsupported File Type")
    stop()
  }
  
  file.remove(TMP)
  
  return(violence_data)
}

save_to_scraping <- function(df, filename) {
  # Final directory
  SCRAPED_DIR = "ScrapedFiles"
  
  # This determines the file path to the document
  # MIGHT NEED TO BE CHANGED TO FIT YOUR WORKING DIRECTORY
  destination = file.path(SCRAPED_DIR, filename)
  save(df, file = destination)
}


mpv_info = c(url  = "https://mappingpoliceviolence.org/s/MPVDatasetDownload.xlsx",
             filename = "mpv.RData")

mpv = scrape_url(mpv_info['url'])
save_to_scraping(mpv, mpv_info['filename'])