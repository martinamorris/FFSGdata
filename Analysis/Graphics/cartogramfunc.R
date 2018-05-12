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


df <- data.frame(permillcalc())
df$State <- factor(df$state) # BM: cartogram fn can't handle characters

# join data frame to the SpatialPolygonsDataFrame
# from https://stackoverflow.com/a/3652472/1036500
usa@data = data.frame(usa@data, df[match(usa@data[,'unique.IDs.'], tolower(df[,'state_name' ])),])

ffcartogram <- function(year){  
  # BM: add a little bit of error handling to help with testing
  if(!year %in% 2000:2017) stop("Please enter a year between 2000 and 2017")
  # BM: add the p here, makes this fn more versatile
  year <- paste0('p', year)
  # make cartogram
  ff_ctgm <- cartogram(usa, year, itermax=5)
  
  # plot it
  tm_shape(ff_ctgm) + 
    tm_fill(year, style="jenks") +
    tm_borders() + 
    tm_layout(frame=F)
  
}

cart2000 <- ffcartogram(2000)
cart2001 <- ffcartogram(2001)
cart2002 <- ffcartogram(2002)
cart2003 <- ffcartogram(2003)
cart2004 <- ffcartogram(2004)
cart2005 <- ffcartogram(2005)
cart2006 <- ffcartogram(2006)
cart2007 <- ffcartogram(2007)
cart2008 <- ffcartogram(2008)
cart2009 <- ffcartogram(2009)
cart2010 <- ffcartogram(2010)
cart2011 <- ffcartogram(2011)
cart2012 <- ffcartogram(2012)
cart2013 <- ffcartogram(2013)
cart2014 <- ffcartogram(2014)
cart2015 <- ffcartogram(2015)
cart2016 <- ffcartogram(2016)
cart2017 <- ffcartogram(2017)