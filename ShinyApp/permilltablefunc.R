library(DT)
library(dplyr)


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