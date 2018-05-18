library(plotly)

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
  
  plot_geo(df, locationmode = 'USA-states') %>%
    add_trace(
      z = ~ x, text = ~hover, locations = ~state,
      color = ~ x, colors = 'Purples'
    ) %>%
    colorbar(title = "Civilian Deaths by Police per Million Capita") %>%
    layout(
      title = 'US Fatal Encounters with Police by State<br>(Hover for breakdown)',
      geo = g
    )
}