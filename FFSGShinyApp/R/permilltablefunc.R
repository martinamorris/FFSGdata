library(DT)
library(dplyr)
source("R/permillcalculation.R")


#' perMillTable
#'
#' Creates a table of fatal encounters by state
#'
#' @param statenm a state name, the state that is displayed or highlighted
#' @param all a boolean, if TRUE table includes all states, if FALSE table only displays the given state
#' @param capita a boolean, if TRUE evaluates cases per million population of each state, if FALSE calculates total cases
#'
#' @seealso \code{\link{permillcalc}} and \code{\link{permilltable}}
#'
#' @export

## @knitr permilltab

permilltable <- function(statenm, all, capita){

  rounded <- permillcalc(capita = capita)

  if(capita){
    rounded[,3:21] <- round(rounded[,3:21], 2)
  }

  if(all){
    DT::datatable(rounded ,  rownames = FALSE) %>% formatStyle(
      'state_name',
      target = 'row',
      backgroundColor = styleEqual(
        statenm, "yellow"
      )
    )
  }else{
    df <- rounded %>%
      filter(state_name == statenm)
    DT::datatable(df,  rownames = FALSE)
  }

}

globalVariables(c("state_name"))
