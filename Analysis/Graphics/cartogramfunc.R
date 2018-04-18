require(maps)
require(sp)
require(maptools)
require(tmap)
require(cartogram)

# get a SpatialPolygonsDataFrame of US states
usa <- map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa <- SpatialPolygonsDataFrame(usa, 
                                data = data.frame(unique(IDs), 
                                                  row.names = unique(IDs)) )

# get a data frame of data with state names. Data here are crime rates,
# built-in dataset

df <- data.frame(permillcalc())

# join data frame to the SpatialPolygonsDataFrame
# from https://stackoverflow.com/a/3652472/1036500
usa@data = data.frame(usa@data, df[match(usa@data[,'unique.IDs.'], tolower(df[,'state_name' ])),])

ffcartogram <- function(year){  
  # make cartogram
  ff_ctgm <- cartogram(usa, year, itermax=5)
  
  # plot it
  tm_shape(ff_ctgm) + 
    tm_fill(year, style="jenks") +
    tm_borders() + 
    tm_layout(frame=F)
}