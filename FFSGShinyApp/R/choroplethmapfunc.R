library(plotly)
source("R/permillcalculation.R")


#' choroplethMap
#'
#' Creates a choropleth map of fatal encounters by state for the given year
#'
#' @param yeart an integer year between 2000 and 2017 or "mean", if blank calculates mean
#'
#' @return a chloropleth map for the given year
#'
#' @seealso \code{\link{plot_geo}}
#'
#' @export

## @knitr choromap

choroplethmap <- function(yeart = "mean"){
  df <- permillcalc()
  x <- df[,colnames(df) == yeart]
  df$hover <- with(df, paste(state_name))
  # give state boundaries a white border
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )

  plotly::plot_geo(df, locationmode = 'USA-states') %>%
    add_trace(
      z = ~df[,colnames(df) == yeart], text = ~hover, locations = ~df$state,
      color = ~df[,colnames(df) == yeart], colors = 'Purples'
    ) %>%
    colorbar(title = "Civilian Deaths by Police per Million Capita") %>%
    layout(
      title = 'US Fatal Encounters with Police by State<br>(Hover for breakdown)',
      geo = g
    )
}
