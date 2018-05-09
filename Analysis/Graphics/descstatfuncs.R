library(dplyr)

descstat <- function(dem){
  if (dem == "Age") {
    df <- fatalencounters%>%
      count(age)
  } 
  if(dem == "Gender") {
    df <- fatalencounters%>%
      count(sex)
  } 
  if(dem == "Race") {
    df <- fatalencounters%>%
      count(race)
  }
  
  return(df)
}

dstable <- function(dem){
  df <- descstat(dem)
  
  datatable(df)
}

dsplot <- function(dem){
  df <- descstat(dem)

  if (dem == "Age") {
    plot <- ggplot(df, aes(age, n))+geom_col()
  } 
  if(dem == "Gender") {
    plot <- ggplot(df, aes(sex, n))+geom_col()
  } 
  if(dem == "Race") {
    plot <- ggplot(df, aes(race, n))+geom_col()
  }
  
  plot
}