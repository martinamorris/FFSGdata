#' ---
#' title: "Scraping data from mappingpoliceviolence.net"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://mappingpoliceviolence.org/>.
library(readxl)

setwd('./ScrapedFiles/')

# mapping police violence, <https://mappingpoliceviolence.org/>


# get xlsx file and write
url <- 'https://mappingpoliceviolence.org/s/MPVDatasetDownload-btzs.xlsx'
download.file(url, destfile = 'MPV_raw.xlsx')

# View
data <- read_xlsx('MPV_raw.xlsx')
glimpse(data)

##################################################################################

##################################################################################