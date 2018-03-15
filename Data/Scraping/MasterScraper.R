currDir <- '~/git/ffsg/Data/Scraping/'
setwd(currDir)
source("MakeKBPData.R")
rm(list = ls())

currDir <- '~/git/ffsg/Data/Scraping/'
setwd(currDir)
source("MakeFEData.R")
rm(list = ls())

currDir <- '~/git/ffsg/Data/Scraping/'
setwd(currDir)
source("MakeMPVData.R")
rm(list = ls())

currDir <- '~/git/ffsg/Data/Scraping/'
setwd(currDir)
source("MakePopData.R")
rm(list = ls())
