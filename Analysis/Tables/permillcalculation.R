library(dplyr)
library(tidyr)
library(rio)
library(stringr)

#------------------------------censusdata---------------------------------------------------
load(here::here("Data/Final/Pop.Rdata"))

reorder_pop_state <- state_pops[order(state_pops$state_abb), ]
pop_state_and_us <- rbind(reorder_pop_state, all_pops[1, ])
pop_state_and_us_mill <- pop_state_and_us[, 3:20] / 1000000

#-----------------------------fatal encounters data-------------------------------------

fatalencounters <-
  rio::import(
    "https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0"
  ) 

#Move variable names from first row to column headers
colnames(fatalencounters) <- fatalencounters[1, ]
fatalencounters <-
  tail(fatalencounters, -1) #remove row with column names
#rename fe colnames to be shorter/match with census data
colnames(fatalencounters)[27] <- "Year"
colnames(fatalencounters)[10] <- "State"

#column has one value but wouldn't let me run with an unnamed column - will prob be fixed by jainul and then I will remove
colnames(fatalencounters)[28] <- "?"
#eventually this will be Jainul's compiled work :)

permillcalc <- function(x = fatalencounters, capita = TRUE){
  
  ##create table of state by year counts
  kdata <- x %>%
    group_by(Year) %>%
    count(State) %>%
    spread(Year, n)
  
  #change NA values (where no data matched) to 0
  kdata[is.na(kdata)] <- 0
  
  #Calculate US total
  kdata["Total" , ][, -1] <- colSums(kdata[, -1])
  kdata[is.na(kdata)] <- "US"
  
  #remove extra column/row (hopefully this won't be an issue once I have jainul's) and remove 2018
  kdata <- kdata[2:53, c(1, 3:20)]
  
  #add full state names
  kdata <- cbind(pop_state_and_us[, 1], kdata)
  colnames(kdata)[1] <- "state_name"
  
  if(capita){
    #calculate deaths per million population
    kpm <- kdata[, -c(1:2)] / pop_state_and_us_mill
    
    #find avegages over all years
    kpm <- cbind(kdata[, 1:2], kpm, rowMeans(kpm))
    
    colnames(kpm)[21] <- "mean"
    table <- kpm
    
  }else{table <- kdata}
  
  colnames(table)[1] <- "state_name"
  colnames(table)[3:20] <- str_c("p",colnames(table)[3:20])
  
  return(table)
}