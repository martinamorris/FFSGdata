#' ---
#' title: "Scraping functions for various online repositories"
#' author: "Martina Morris"
#' author: "Vaughn Johnson"
#' date: '`r format(Sys.Date(), "%Y-%m-%d")`'
#' ---

#' A compliation of functions which scrape data from Mapping Police Violence,
#' Killed By Police, Fatal Encounters, and the Census.
#' 
#'

#' Always assume we're stating in the ffsg top level directory

#' Get data from Census, KBP, FE, and MPV
#' 
#' @return Void. Adds all scraped data to /ScrapedFiles/ dir
#' @exportdocument()

pwd = file.path("Data",
                "ffsgData",
                "R")

scrape_all_data <- function() {
  
  #' Census Data
  source(file.path(pwd, "MakePopData.R"))
  state_urls  = c("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/state/st-est00int-01.xls",
                 "https://www2.census.gov/programs-surveys/popest/tables/2010-2017/state/totals/nst-est2017-01.xlsx")
  county_urls = c("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv",
                  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/counties/totals/co-est2016-alldata.csv")
  pop_save_file = file.path(pwd, "ScrapedFiles", "Pop.RData")
  scrape_population_data(state_urls, county_urls, pop_save_file)
  
  #' Mapping Police Violence
  source(file.path(pwd, "MakeMPVData.R"))
  url = 'https://mappingpoliceviolence.org/s/MPVDatasetDownload.xlsx'
  mpv_save_file = file.path(pwd, "ScrapedFiles", "MPV.clean.Rdata")
  scrape_MPV_data(url, mpv_save_file)
  
  #' Fatal Encounters
  source(file.path(pwd, "MakeFEData.R"))
  url = 'https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/export?format=csv&id=1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE&gid=0'
  fe_save_file = file.path(pwd, "ScrapedFiles", "fe.clean.Rdata")
  scrape_FE_data(url, fe_save_file)
  
  #' Killed By Police
  source(file.path(pwd, "MakeKBPData.R"))
  kbp_save_file = file.path(pwd, "ScrapedFiles", "KBP.clean.Rdata")
  scrape_KBP_data(kbp_save_file)
}

scrape_all_data()
