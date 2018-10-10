library(DT)
library(dplyr)

#' perMillTable
#' 
#' Creates a table of fatal encounters by state
#' 
#' @param state a state name, the state that is displayed or highlighted
#' @param all a boolean, if TRUE table includes all states, if FALSE table displays just the given state
#' @param capita a boolean, if TRUE evaluates cases per million population of each state, if FALSE just calculates total cases
#' 
#' @seealso \code{\link{permillcalc}} and \code{\link{permilltable}}
#' 
#' @export
permilltable <- function(state, all, capita){
  rounded <- permillcalc(capita = capita)
  if(capita){
    rounded[,3:21] <- round(rounded[,3:21], 2)
  }
  if(all){
    datatable(rounded) %>% formatStyle(
      'state_name',
      target = 'row',
      backgroundColor = styleEqual(
        state, "yellow"
      )
    )
  }else{
    df <- rounded %>%
      filter(state_name == state)
    datatable(df)
  }
  
}