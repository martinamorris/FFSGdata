library(dplyr)
library(tidyr)
#library(leaflet)
library(ggplot2)
library(DT)
library(usmap)
library(stringr)
library(sp)
# library(maps)
# library(maptools)
library(UScensus2010)
library(openintro)
data("countyfips")
source("R/permillcalculation.R")

load("extdata/fe.clean.Rdata")
fatalencounters <- fe.clean

x = fatalencounters

get_state <- function(staten){
  festate <- fatalencounters  %>%
    filter(state == staten)
  return(festate)
}

statemap <- function(staten){
  #filters data for state
  festate <- get_state(staten)
  festate$county <- tolower(festate$county)
  cfips <- as.data.frame(countyfips)
  cfips$acronym <-  toupper(cfips$acronym)
  cfips$countyname <- str_remove(cfips$countyname, " county")
  filtered <- cfips %>% filter(acronym == staten)


  countyData <- inner_join(festate, filtered, by = c("county" = "countyname")) %>%
    group_by(fips, county) %>%
    count()

  plot_usmap(data = countyData, values ="n", include = staten, lines = "red") +
    scale_fill_continuous( low = "white", high = "red", name = "Fatal encounter", label = scales::comma) +
    labs(title = abbr2state(staten))+
    theme(legend.position = "left")
}









f_years <- function(staten) {
  festate <- get_state(staten)
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

datatable(top10)
ggplot(top10, aes(city, Total))+geom_bar(stat = "identity")

}






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

table_metrics <- function(staten){

  count <- permillcalc(capita = FALSE)
  capita <- permillcalc(capita = TRUE)

  state_capita <- capita[which(capita$state == staten),]
  state_counts <- count[which(count$state == staten),]

  Metric <- c("In 2017", "Since 2000", "Rank")
  Capita <- c(state_capita$p2017, state_capita$mean, getrank(staten, capita = TRUE))
  Counts <- c(state_counts$p2017, state_counts$Total, getrank(staten, capita = FALSE))

  Metrics <- data.frame(Metric, Counts, Capita)

  datatable(Metrics, rownames = FALSE)
}




# leaflet(festate) %>% addTiles() %>%
#   addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())

#Create plot
linegraph_state <- function(filter, staten){
  if(filter == "none") {
    permillgraph(staten, FALSE,FALSE)
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
#
# genderList <- unique(festate$sex)
#
# perGender<-  function(gendern){
#   nrow (festate %>% filter(sex == gendern))
# }
#
# genderDf <-  data.frame(gender = genderList)
# genderDf$Total <-  sapply(genderDf$gender, perGender)
# datatable(genderDf, rownames = FALSE)

perGender <- function(staten){
  festate <- get_state(staten)
  genderList <- unique(festate$sex)

  gender<-  function(gendern){
    nrow (festate %>% filter(sex == gendern))
  }
  genderDf <-  data.frame(gender = genderList)
  genderDf$Total <-  sapply(genderDf$gender, gender)
  datatable(genderDf, rownames = FALSE)
}


#Race




perRace <- function(staten){
  festate <- get_state(staten)
  raceList <- unique(festate$race)
  race <- function(nrace){
    nrow (festate %>% filter(race == nrace))
  }
  raceDf <-  data.frame(race = raceList)
  raceDf$total <- sapply(raceDf$race, race)
  datatable(raceDf, rownames = FALSE)
}



# child 0-14
#Youth : 15-24years
#Adults :25-64
#Seniors : + 65years






perAge <- function(staten){
  festate <- get_state(staten)
  fage <- function(range1, range2){
    nrow(festate %>% filter(age > range1, age < range2))
  }
  ageDF <- data.frame(age = c("child: 0-14 years","Youth: 15-24years ","Adults :25-64","Seniors : + 65years")
                      , Total = c(fage(0,14), fage(15,24), fage(25,64), fage(65,104)))
  datatable(ageDF, rownames = FALSE)
}




globalVariables(c("year"))
