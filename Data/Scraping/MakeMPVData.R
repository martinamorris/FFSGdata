#' ---
#' title: "Scraping data from mappingpoliceviolence.net"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://mappingpoliceviolence.org/>.
library(readxl)
library(dplyr)


#' Scrape the MPV database
#' 
#' @param url URL for MPV spreadsheet
#' @param save_file filename to be saved in /ScrapedFiles/
#' @return Void. Saves the data to `save_file`.
#' @export
scrape_MPV_data <- function(url, save_file) {
  save_file = file.path("./ScrapedFiles", save_file)
  TMP = "MPVTEMP"
  # get xlsx file and write
  download.file(url, destfile = TMP)
  
  # View
  mpv <- read_xlsx(TMP)
  file.remove(TMP)
  
  save.image("./MPV.clean.Rdata")
}