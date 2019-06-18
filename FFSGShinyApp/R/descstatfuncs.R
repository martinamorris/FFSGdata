library(dplyr)
library(ggplot2)

#' descStat
#'
#' Stratifies a data set based on a given demographic
#' Used within functions dstable and dsplot
#'
#' @param dem a demographic, the demographic the data is to be stratified for (Age, Gender, or Race)
#' @param ds a data frame, data frame you want to be stratified, if no input uses fatalencounters
#'
#' @return stratified data frame
#'
#' @seealso \code{\link{dstable}} and \code{\link{dsplot}}
#'
#' @importFrom magrittr "%>%"
#' @export

## @knitr descstat

descstat <- function(dem, ds = fatalencounters){
  if (dem == "Age") {
    df <- ds%>%
      count(age)
  }
  if(dem == "Gender") {
    df <- ds%>%
      count(sex)
  }
  if(dem == "Race") {
    df <- ds%>%
      count(race)
  }

  return(df)
}

#' dsTable
#'
#' Makes a table based off stratified data
#'
#' @param dem a demographic, the demographic the data is to be stratified for (Age, Gender, or Race)
#'
#' @return data table of the resulting data from an in-method descstat call
#'
#' @seealso \code{\link{descstat}} and \code{\link{dsplot}}
#'
#' @export

## @kintr dstab

dstable <- function(dem){
  df <- descstat(dem)

  datatable(df,  rownames = FALSE)
}

#' dsPlot
#'
#'  Makes a table based off stratified data
#'
#' @param dem a demographic, the demographic the data is to be stratified for (Age, Gender, or Race)
#'
#' @return ggplot of the resulting data from an in-method descstat call
#'
#' @seealso \code{\link{descstat}} and \code{\link{dstable}} and \code{\link{ggplot2}}
#'
#' @export

## @knitr dsplt

dsplot <- function(dem){
  df <- descstat(dem)

  if (dem == "Age") {
    plot <- ggplot2::ggplot(df, aes(age, n))+geom_col()
  }
  if(dem == "Gender") {
    plot <- ggplot2::ggplot(df, aes(sex, n))+geom_col()
  }
  if(dem == "Race") {
    plot <- ggplot2::ggplot(df, aes(race, n))+geom_col()
  }

  plot
}

globalVariables(c("age", "sex", "race"))
