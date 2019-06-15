library(tidyr)
library(dplyr)

#------------------------------censusdata---------------------------------------------------

## @knitr popdata

load("extdata/Pop.Rdata")
reorder_pop_state <- state_pops[order(state_pops$state_abb), ]
pop_state_and_us <- rbind(reorder_pop_state, all_pops[1, ])
pop_state_and_us_mill <- pop_state_and_us[, 3:20] / 1000000

#-----------------------------fatal encounters data-------------------------------------

## @knitr fedata

load("extdata/fe.clean.Rdata")
fatalencounters <- fe.clean
#------------------------------function-----------------------------------------------

#' PerMillCalc
#'
#' Calculates the fatal encounters (per million population or total) for each state
#'
#' @param x a data frame of individual fatal encounter cases, default uses fatalencounters
#' @param capita a boolean, if TRUE evaluates cases per million population of each state, if FALSE calculates total cases, default is TRUE
#' @return table a data frame of fatal encounters per state (evaluated per capita or as totals based on capita parameter)
#'
#' @export

## @knitr permcalc

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
    kpm <- kdata[, -c(1:2,21:22)] / pop_state_and_us_mill

    #find avegages over all years
    kpm <- cbind(kdata[, 1:2], kpm, rowMeans(kpm))

    colnames(kpm)[21] <- "mean"
    table <- kpm

  }else{
    table <- cbind(kdata, rowSums(kdata[,3:20]))
    colnames(table)[colnames(table)=="rowSums(kdata[, 3:20])"] <- "Total"
  }

  colnames(table)[1] <- "state_name"
  colnames(table)[3:20] <- stringr::str_c("p",colnames(table)[3:20])

  return(table)
}

