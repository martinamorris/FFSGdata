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

#fatalencounters <- read.csv("Data/Final/fe_clean_lat_long.csv") 

#------------------------------function-----------------------------------------------

permillcalc <- function(x = fatalencounters, capita = TRUE){
  
  ##create table of state by year counts
  kdata <- x %>%
    group_by(year) %>%
    count(state) %>%
    spread(year, n)
  
  #change NA values (where no data matched) to 0
  kdata[is.na(kdata)] <- 0
  
  #Calculate US total
  kdata["Total" , ][, -1] <- colSums(kdata[, -1])
  kdata$state <- as.character(kdata$state)
  kdata[52,1] <- "US"
  
  
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