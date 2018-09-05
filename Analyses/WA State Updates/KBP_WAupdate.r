# WA state updates for KBP

library(tidyr)
library(tidyverse)

mydir <- getwd()
source(paste(mydir, '/Data/Scraping/MakeKBPData.R', sep="/"))

thismonth <- as.numeric(format(Sys.Date(), '%m'))-1 # row index for last complete month

kbpwa.MY <- filter(kbp, state == "WA") %>%
  count(month, year) %>%
  spread(year, n, fill=0)

thisyear <- as.numeric(ncol(kbpwa.MY)) # column index for current year


kbpwa.cumsum <- cbind("month"=1:12, mutate_all(kbpwa.MY[,2:thisyear], funs(cumsum)))

# total to this point, by year
print.data.frame(kbpwa.cumsum[thismonth,])

## PLOTS

# Monthly totals by month and year

matplot(x=kbpwa.MY[,1], y=kbpwa.MY[,2:(thisyear-1)], 
        type='l', col='grey', pch='*', lty=3,
        main="KBP Monthly Fatal Police Killings in WA State: 2013-2018",
        xlab = "Month", ylab = "Monthly Number Killed",
        xaxt = "n", cex.axis = 0.7)

axis(1, at=1:12, 
     labels=c('Jan','Feb','Mar','Apr','May','Jun',
              'Jul','Aug','Sep','Oct','Nov','Dec'),
     cex.axis=0.7)

matlines(x=kbpwa.MY[,1], y=apply(kbpwa.MY[,2:(thisyear-1)], 1, 'mean'),
         type='l', col='grey', lty=3, lwd=3)

matlines(x=kbpwa.MY[1:thismonth,1], y=kbpwa.MY[1:thismonth,thisyear],
         type='l', col='red', pch='*', lty=1, lwd=2)

legend("topleft", cex=0.6,
       legend = c("Individual Years", 
                  paste("Monthly Average: 2013-", 2013+thisyear-3, sep=""),
                  2013+thisyear-2),
       lty = c(3,3,1), lwd = c(1,2,2),
       col = c("grey","black","red"))

# Cumulative totals by month and year

matplot(x=1:12, y=kbpwa.cumsum[,2:thisyear], 
        type='l', col='gold3', pch='*', lty=3,
        ylim = c(0, max(kbpwa.cumsum)),
        main=paste("KBP Cumulative Fatal Police Killings by Month in WA State: 2000-",
                   2013+thisyear-2, sep=""),
        xlab = "Month", ylab = "Cumulative Number Killed",
        xaxt = "n")

axis(1, at=1:12, 
     labels=c('Jan','Feb','Mar','Apr','May','Jun',
              'Jul','Aug','Sep','Oct','Nov','Dec'),
     cex.axis=0.7)

matlines(x=1:12, y=apply(kbpwa.cumsum[2:thisyear], 1, 'mean'),
         type='l', lty=3, lwd=3)

matlines(x=1:thismonth, y=kbpwa.cumsum[1:thismonth,thisyear],
         type='l', col='red', lty=1, lwd=2)

legend("topleft", cex=0.6,
       legend = c("Individual Years", 
                  paste("Monthly Average: 2013-", 2013+thisyear-3, sep=""),
                  2013+thisyear-2),
       lty = c(3,3,1), lwd = c(1,2,2),
       col = c("grey","black","red"))

kbpwa.current <- filter(kbp, state == "WA" & year == 2018)
write_excel_csv(kbpwa.current, path="F:/GitHub/FFSG/ffsg/Data/WAUpdates/kbpwa.current.csv")
