library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(DT)
library(usmap)

library(sp)
library(maps)
library(maptools)

source("R/permillcalculation.R")


load("extdata/fe.clean.Rdata")
fatalencounters <- fe.clean

x = fatalencounters

#filters data for state
festate <- x %>%
  filter(state == "WA")

#finds fatal encounters by race over years
state_race <- festate %>%
  group_by(year) %>%
  count(race) %>%
  spread(year, n)
state_race[is.na(state_race)] <- 0 #replaces NA with 0

#finds fatal encounters by race over years
state_gender <- festate %>%
  group_by(year) %>%
  count(sex) %>%
  spread(year, n)
state_gender[is.na(state_gender)] <- 0 #replaces NA with 0

state_city <- festate %>%
  group_by(year) %>%
  count(city) %>%
  spread(year, n)
state_city[is.na(state_city)] <- 0 #replaces NA with 0
state_city$Total = rowSums(state_city[,2:20])

top10 <- state_city %>%
  arrange(desc(Total)) %>%
  top_n(10, Total) %>%
  select(city, Total)

top10dt <- datatable(top10)

top10_bar <- ggplot(top10, aes(city, Total))+geom_bar(stat = "identity")

##get rank function
getrank <- function(stateabbr, capita = TRUE) {
  festates <- permillcalc(capita=capita)
  if(capita == TRUE) {
    festates <- festates %>%
      select(state, mean) %>%
      arrange(desc(mean))
  } else {
    festates <- festates %>%
      select(state, Total) %>%
      arrange(desc(Total))
  }
  rank <- which(festates$state == stateabbr)
  return(rank)
}


## Create Table of Metrics

count <- permillcalc(capita = FALSE)
capita <- permillcalc(capita = TRUE)

state_capita <- capita[which(capita$state == "WA"),]
state_counts <- count[which(count$state == "WA"),]

Metric <- c("In 2017", "Since 2000", "Rank")
Capita <- c(state_capita$p2017, state_capita$mean, getrank("WA", capita = TRUE))
Counts <- c(state_counts$p2017, state_counts$Total, getrank("WA", capita = FALSE))

Metrics <- data.frame(Metric, Counts, Capita)

Metric_DT <- datatable(Metrics)

##Choropleth

countyData <- fatalencounters %>%
  filter(state == "WA", county != "Clallam Bay") %>%
  group_by(county) %>%
  count()

countyData$county <- fips("WA", county = countyData$county)
colnames(countyData) <- c("fips", "number_values")

plot_usmap( data = countyData , values = "number_values", include = "WA", lines = "red") +
  scale_fill_continuous( low = "white", high = "red", name = "Fatal encounter", label = scales::comma) +
  labs(title = "Washington state")

##Map

leaflet(festate) %>% addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())

#Create plot
linegraph_state <- function(filter){
  if(filter == "none") {
    permillgraph("Washington", FALSE,FALSE)
  }else if(filter == "race") {
    matplot(2000:2017, t(state_race[, 2:19]), type = c("b"),pch=1,col = 1:6, ylab = "Fatal Encounters", xlab = "Year") #plot
    legend("topleft", legend = c("African-American/Black", "Asian/Pacific Islander", "European-American/White", "Hispanic/Latino", "Native American/Alaskan", "Unspecified"), col=1:6, pch=1) # optional legend
  }else{
    matplot(2000:2017, t(state_gender[, 2:19]), type = c("b"),pch=1,col = 1:3, ylab = "Fatal Encounters", xlab = "Year") #plot
    legend("topleft", legend = c("Female", "Male", "Unspecified"), col=1:3, pch=1) # optional legend
  }
}

# Demographic breakdown


#Gender

genderList <- unique(festate$sex)

perGender<-  function(gender){
  nrow (festate %>% filter(sex == gender))
}

genderDf <-  data.frame(gender = genderList)
genderDf$Total <-  sapply(genderDf$gender, perGender)
datatable(genderDf, rownames = FALSE)



#Race

raceList <- unique(festate$race)

perRace <- function(nrace){
  nrow (festate %>% filter(race == nrace))
}

raceDf <-  data.frame(race = raceList)
raceDf$total <- sapply(raceDf$race, perRace)
datatable(raceDf, rownames = FALSE)



# child 0-14
#Youth : 15-24years
#Adults :25-64
#Seniors : + 65years


perAge <- function(range1, range2){
  nrow(festate %>% filter(age > range1, age < range2))
}

ageDF <- data.frame(age = c("child: 0-14 years","Youth: 15-24years ","Adults :25-64","Seniors : + 65years")
                    , Total = c(perAge(0,14),perAge(15,24),perAge(25,64),perAge(65,104)))
datatable(ageDF, rownames = FALSE)



#test counties



latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county (plus DC, minus HI & AK)
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, counties_sp)

  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
test<- festate %>% select(x = longitude, y= latitude)

nameCountie <- as.data.frame(latlong2county(test))

festate$newName <- nameCountie

new <- festate %>% select(race, year, age, sex, newName)












globalVariables(c("year"))
