library(leaflet)

interactivemap <- leaflet(data = fatalencounters, width = "100%") %>% addTiles() %>%
  addMarkers( ~ as.double(fatalencounters$Longitude),
              ~ as.double(fatalencounters$Latitude),
              popup = ~ fatalencounters$`Location of death (city)`,
              label = ~ fatalencounters$`Subject's name`,
              clusterOptions = markerClusterOptions())
