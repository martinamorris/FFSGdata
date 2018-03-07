library(dplyr)
library(tidyr)
library(rio)

#---------------------------------read in datasets----------------------------------------

censusdata <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2010-2016/state/totals/nst-est2016-01.xlsx")
fatalencounters <- rio::import("https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0")




#---------------------------------organize census data------------------------------------

#Only use rows with states or us data
censusdatastates <- tail(head(censusdata, 59), 51) #uses just state data
#remove period in front of state name and change state names to abbreviations to match FE data
censusdatastates[,1] <- state.abb[match(gsub("\\.", "", censusdatastates[,1]), state.name)]
censusdatastates[is.na(censusdatastates)] <- "DC" #doesn't recognize DC as state so change manually
censusdatastates <- censusdatastates[order(censusdatastates$`table with row headers in column A and column headers in rows 3 through 4. (leading dots indicate sub-parts)`),]

#pull US data and combine
unitedstates <- tail(head(censusdata, 4), 1) #us data
censusdata <- rbind(censusdatastates, unitedstates) #both us and state

#remove columns that don't relate to a specific year or the state column and add variable names
censusdata <- censusdata[,c(1,4:10)]
names(censusdata) <- c('State', 2010:2016) #adds labels

#columns read in as type 'character' change to type 'integer'
censusdata$'2010' <- as.integer(censusdata$'2010')
censusdata$'2011' <- as.integer(censusdata$'2011')
censusdata$'2012' <- as.integer(censusdata$'2012')
censusdata$'2013' <- as.integer(censusdata$'2013')
censusdata$'2014' <- as.integer(censusdata$'2014')
censusdata$'2015' <- as.integer(censusdata$'2015')
censusdata$'2016' <- as.integer(censusdata$'2016')
                                
censusdata[,-1] <- censusdata[,-1]/1000000 #finds population in millions




#-----------------------------organize fatal encounters data-------------------------------------

#Move variable names from first row to column headers
colnames(fatalencounters) <- fatalencounters[1,]
fatalencounters <- tail(fatalencounters, -1) #remove row with column names
#rename fe colnames to be shorter/match with census data
colnames(fatalencounters)[27] <- "Year"
colnames(fatalencounters)[10] <- "State"

#column has one value but wouldn't let me run with an unnamed column - will prob be fixed by jainul and then I will remove
colnames(fatalencounters)[28] <- "?"

##create table of state by year counts
fedata <- fatalencounters %>%
  group_by(Year)%>%
  count(State)%>%
  spread(Year, n)

#change NA values (where no data matched) to 0
fedata[is.na(fedata)] <- 0
#Calculate US total
fedata["Total" ,][,-1] <- colSums(fedata[,-1])
fedata[is.na(fedata)] <- "ustotal"

#edit fedata to be the same size as censusdata
fedata<- fedata[2:53,c(1,13:19)]



#------------------------------calculate deaths per million------------------------------

#calculate deaths per million population
fepermill <- fedata[,-1] / censusdata[,-1]

#find avegages over all years
rownames(fepermill) <- fedata$State
fepermill$average <- rowMeans(fepermill)
