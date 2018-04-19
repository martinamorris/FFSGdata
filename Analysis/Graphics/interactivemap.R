library(leaflet)

interactivemap <- leaflet(data = fatalencounters, width = "100%") %>% addTiles() %>%
  addMarkers( ~ as.double(fatalencounters$Longitude),
              ~ as.double(fatalencounters$Latitude),
              popup = ~ paste(fatalencounters$`Subject's name`, fatalencounters$`Location of death (city)`, fatalencounters$`Date&Description`, sep = ", "),
              label = ~ fatalencounters$`Subject's name`,
              clusterOptions = markerClusterOptions())
