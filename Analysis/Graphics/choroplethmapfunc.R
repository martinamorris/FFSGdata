library(plotly)

choroplethmap <- function(yeart){  
  x = fepermill[,colnames(fepermill) == yeart]
  fepermill$hover <- with(fepermill, paste(state_name))
  # give state boundaries a white border
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  plot_geo(fepermill, locationmode = 'USA-states') %>%
    add_trace(
      z = ~ x, text = ~hover, locations = ~State,
      color = ~ x, colors = 'Purples'
    ) %>%
    colorbar(title = "Civilian Deaths by Police per Million Capita") %>%
    layout(
      title = 'US Fatal Encounters with Police by State<br>(Hover for breakdown)',
      geo = g
    )
}