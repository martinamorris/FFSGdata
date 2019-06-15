library(leaflet)

## @knitr intmap

interactivemap <- leaflet::leaflet(data = fatalencounters, width = "100%") %>% addTiles() %>%
  addMarkers( ~ as.double(fatalencounters$longitude),
              ~ as.double(fatalencounters$latitude),
              popup = ~ paste(fatalencounters$name, fatalencounters$city, fatalencounters$Description, sep = ", "),
              label = ~ fatalencounters$name,
              clusterOptions = markerClusterOptions())
