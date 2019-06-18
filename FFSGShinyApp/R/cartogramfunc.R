require(maps)
require(sp)
require(maptools)
require(tmap)
require(cartogram)

# get a SpatialPolygonsDataFrame of US states
usa <- maps::map("state", fill = TRUE, plot = FALSE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- maptools::map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa <- sp::SpatialPolygonsDataFrame(usa,
                                data = data.frame(unique(IDs),
                                                  row.names = unique(IDs)) )


df <- data.frame(permillcalc())
df$State <- factor(df$state) # BM: cartogram fn can't handle characters

# join data frame to the SpatialPolygonsDataFrame
# from https://stackoverflow.com/a/3652472/1036500
usa@data = data.frame(usa@data, df[match(usa@data[,'unique.IDs.'], tolower(df[,'state_name' ])),])


#' ffCartogram
#'
#' Creates a cartogram of fatal encounters per capita by state for the given year
#'
#' @param year an integer year between 2000 and 2017
#'
#' @return a cartogram map for the given year
#'
#' @seealso \code{\link{cartogram_cont}}
#'
#' @export
ffcartogram <- function(year){
  # BM: add a little bit of error handling to help with testing
  if(!year %in% 2000:format(Sys.Date(), "%Y")) stop("Please enter a year between 2000 and 2017")
  # BM: add the p here, makes this fn more versatile
  year <- paste0('p', year)
  # make cartogram
  ff_ctgm <- cartogram::cartogram_cont(usa, year, itermax=5)

  # plot it
  return(
  tmap::tm_shape(ff_ctgm) +
    tm_fill(year, style="jenks") +
    tm_borders() +
    tm_layout(frame=F)
  )

}
