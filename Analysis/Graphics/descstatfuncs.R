library(dplyr)

descstat <- function(dem){
  if (dem == "Age") {
    df <- fatalencounters%>%
      count(`Subject's age`)
  } 
  if(dem == "Gender") {
    df <- fatalencounters%>%
      count(`Subject's gender`)
  } 
  if(dem == "Race") {
    df <- fatalencounters%>%
      count(`Subject's race`)
  }
  
  return(df)
}

dstable <- function(dem){
  df <- descstat(dem)
  
  datatable(df)
}

dsplot <- function(dem){
  df <- descstat(dem)

  barplot(df$n, names.arg = df[,1])
  }