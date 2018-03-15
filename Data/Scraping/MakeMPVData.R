#' ---
#' title: "Scraping data from mappingpoliceviolence.net"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://mappingpoliceviolence.org/>.
library(readxl)
library(dplyr)

setwd('./ScrapedFiles/')

# mapping police violence, <https://mappingpoliceviolence.org/>


# get xlsx file and write
url <- 'https://mappingpoliceviolence.org/s/MPVDatasetDownload-btzs.xlsx'
download.file(url, destfile = 'MPV.xlsx')

# View
data <- read_xlsx('MPV.xlsx')
file.remove("MPV.xlsx")

glimpse(data)

save.image("./MPV.clean.Rdata")