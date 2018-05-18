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

whichcart <- function(year){
  #error message - year must be within bounds
  if(!year %in% 2000:2017) stop("Please enter a year between 2000 and 2017")
  
  if(year==2000){return(cart2000)}
  if(year==2001){return(cart2001)}
  if(year==2002){return(cart2002)}
  if(year==2003){return(cart2003)}
  if(year==2004){return(cart2004)}
  if(year==2005){return(cart2005)}
  if(year==2006){return(cart2006)}
  if(year==2007){return(cart2007)}
  if(year==2008){return(cart2008)}
  if(year==2009){return(cart2009)}
  if(year==2010){return(cart2010)}
  if(year==2011){return(cart2011)}
  if(year==2012){return(cart2012)}
  if(year==2013){return(cart2013)}
  if(year==2014){return(cart2014)}
  if(year==2015){return(cart2015)}
  if(year==2016){return(cart2016)}
  if(year==2017){return(cart2017)}
}