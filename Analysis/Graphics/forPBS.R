library(dplyr)
library(fuzzyjoin)
library(readr)

setwd("C:/Users/morrism/Dropbox/FatalForce/Data") 

#=======US/WA/KC/Rest of WA/Seattle comparison===============

# note that the FE and ST datasets record no KC/SEA killings in 2008, but
# the SPD shows 5 killings in that year.  for now i add the 5 killings
# back to the 2008 FE KC/SEA datasets

#FATAL ENCOUNTERS

fe.clean.all <- read.csv(file="FatalEncounters/fe.clean.csv")

#FE US
fe.us=group_by(fe.clean.all,year) %>% summarise(Number=n())

#FE WA state
fe.wa.all<- fe.clean.all[fe.clean.all$state=="WA",]
fe.wa.bycounty=group_by(fe.wa.all,county,year) %>% summarise(Number=n())


#FE King County
## 2008 imputed from SPD
fe.king <- rbind(fe.wa.bycounty[fe.wa.bycounty$county=="King",-1], 
                 c(2008,0))
fe.king <- fe.king[order(fe.king$year),]

#FE Rest of Washington
## note we need to delete a row with NA
fe.rest <- fe.wa[fe.wa.bycounty!="King",]
fe.rest <- group_by(fe.rest, year) %>% summarise(Number = sum(Number))
fe.rest <- fe.rest[-19,]

#FE Seattle 
## 2008 imputed from SPD, 2017 added as 0
fe.seattle.clean.data<- fe.wa.all[fe.wa.all$city=="Seattle",]
fe.seattle <- rbind(group_by(fe.seattle.clean.data,year) %>% 
                      summarise(Number=n()),
                   c(2008,0)) 
#                     c(2017,0))
fe.seattle <- fe.seattle[order(fe.seattle$year),]

#SEATTLE TIMES

st.data.all <- read.csv("SeattleTimes/st.clean.csv")

#ST King County
# 2008 imputed from SPD
st.data.bycounty=group_by(st.data.all,county,year) %>% summarise(Number=n())
st.king <- rbind(st.data.bycounty[st.data.bycounty$county=="King",-1],
              c(2008,0))
st.king <- st.king[order(st.king$year),]


#ST Rest of Washington
st.rest=st.data.bycounty[st.data.bycounty$county!="King",]
st.rest=group_by(st.rest, year) %>% summarise(Number = sum(Number))

##SPD

library(lubridate)

spd.all<-read_csv("SPD_Officer_Involved_Shooting_OIS_Data.csv")
aaa <- as.Date(spd.all$Date, "%m/%d/%Y")
spd.yr <- year(aaa)
spd.all <- cbind(spd.all,year=spd.yr)

#Killings in Seattle (omitting Tukwila and Sturgis)
# note this includes non-fatal shootings
spd.seattle.all <- spd.all[spd.all$City=="Seattle",]
with(spd.seattle.all, table(year,Fatal))
spd.seattle.ois <- group_by(spd.seattle.all,year) %>% summarise(Number=n())

spd.seattle.fatal <- spd.seattle.all[spd.all$Fatal=="Yes",]
spd.seattle=rbind(group_by(spd.seattle.fatal,year) %>% summarise(Number=n()),
                  c(2008,0))
spd.seattle <- spd.seattle[order(spd.seattle$year),]



## Temp population data
pop <- readxl::read_excel("PopulationData/tempWA2005_2016.xlsx")

fecomp.by.yr <- data.frame(2000:2017, 
                      fe.seattle[,2], 
                      fe.king[,2], 
                      fe.rest[,2], 
                      fe.wa[,2], 
                      fe.us[,2])
colnames(fecomp.by.yr) <- c('year','seattle','king','rest','wa','us')
fe.comp.2005.2016 <- fecomp.by.yr[fecomp.by.yr$year>2004 
                                  & fecomp.by.yr$year<2017,]
rownames(fe.comp.2005.2016)<- rownames(pop)

fe.kpm.2005.2016 <- cbind(year=2005:2016,
                          fe.comp.2005.2016/pop)[,-2]

# Seattle estimates:  FE vs. SPD

seattle <- cbind(year=2005:2016, 
                 fe=fe.comp.2005.2016$seattle, 
                 spd=spd.seattle$Number)
matplot(x=2005:2016, seattle[,2:3], type="b", pch=c("f","s"),
        main="Seattle: Fatal Encounters v SPD")

#========Police Killings in King County, Seattle, and the Rest of WA plot (unsmoothed)===================================
library(DescTools)

# all comparisons
matplot(2005:2016, fe.kpm.2005.2016[,2:6], type="b", pch="12345")

legend(x=2005, y=12, 
       legend=colnames(fe.kpm.2005.2016)[2:6], 
       lty=1:5, pch="12345", col=1:5)

# just SEA, WA and US

plot(2005:2016, fe.kpm.2005.2016$seattle, type="n",
     main="Comparison of Seattle, WA State and the US",
     xlab="Year", ylab="Killings per million persons")


legend(x=2005, y=12, 
       legend=c("Seattle", "WA State", "US"), 
       pch=16, col=c("steelblue", "gold","gray60"))

lines(loess(seattle~year, data=fe.kpm.2005.2016), col="steelblue")
lines(loess(wa~year, data=fe.kpm.2005.2016), col="gold")
lines(loess(us~year, data=fe.kpm.2005.2016), col="gray60")

# raw numbers
plot(fe.seattle$year, fe.seattle$Number, type="b", col="red", lwd=2,
     ylim=c(0,16), xlim=c(2000,2016),
     xaxp = c(2000, 2016, 16), yaxp = c(0, 16, 16),
     main="Estimates of totals shot, and killed by police in Seattle",
     xlab="Year", ylab="Estimated number")
lines(spd.seattle$year, spd.seattle$Number, type="b", col="blue", lwd=2)
lines(spd.seattle.ois$year, spd.seattle.ois$Number, type="b", col="green", lwd=2)

abline(v=c(2000:2016), col="lightgrey")
abline(h=c(0:13), col="lightgrey")
legend(2000, 15.5 , c("SPD OIS", 
                  "SPD Fatal OIS", 
                  "Fatal Encounters (SEA)"),
       bg="white", box.col="white",
       col=c("green", "blue", "red"), lty=1, pch="o", cex=0.8)

