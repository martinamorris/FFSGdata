# WA state updates for Fatal Encounters

library(tidyr)

mydir <- getwd()
source(paste(mydir, '/Data/Scraping/MakeFEData.R', sep="/"))

thismonth <- 5

tmp.data <- separate(fe.clean, dateDMY,
                     into = c('month', 'day','year'),
                     sep = "/",
                     remove = FALSE,
                     convert = TRUE)

fewa.MY <- filter(tmp.data, state == "WA") %>%
  count(month, year) %>%
  spread(year, n, fill=0)

fewa.cumsum <- mutate_all(fewa.MY[,2:20], funs(cumsum))

# total to this point, by year
apply(fewa.MY[1:5,], 2, "sum")

## PLOTS

# Monthly totals by month and year

matplot(x=fewa.MY[,1], y=fewa.MY[,2:11], 
        type='l', col='gold3', pch='*', lty=3,
        ylim = c(0, max(fewa.MY[,2:20])),
        main="Monthly Fatal Police Killings in WA State: 2000-2018",
        xlab = "Month", ylab = "Monthly Number Killed",
        xaxt = "n", cex.axis = 0.7)
matlines(x=fewa.MY[,1], y=fewa.MY[,12:20], 
        type='l', col='grey', pch='*', lty=3)
        
axis(1, at=1:12, 
     labels=c('Jan','Feb','Mar','Apr','May','Jun',
              'Jul','Aug','Sep','Oct','Nov','Dec'),
     cex.axis=0.7)

matlines(x=fewa.MY[,1], y=apply(fewa.MY[,2:11], 1, 'mean'),
        type='l', col='gold4', lty=3, lwd=3)
matlines(x=fewa.MY[,1], y=apply(fewa.MY[,12:20], 1, 'mean'),
         type='l', col='black', lty=3, lwd=3)

matlines(x=fewa.MY[1:thismonth,1], y=fewa.MY[1:thismonth,20],
         type='l', col='red', pch='*', lty=1, lwd=2)

legend("topleft", cex=0.6,
       legend = c("Individual Years", 
                  "Monthly Average, 2010's",
                  "Monthly Average, 2000's",
                  "2018"),
       lty = c(3,3,3,1), lwd = c(1,2,2,2),
       col = c("grey","black","gold4","red"))

# Cumulative totals by month and year

matplot(x=1:12, y=fewa.cumsum[,1:10], 
        type='l', col='gold3', pch='*', lty=3,
        ylim = c(0, max(fewa.cumsum)),
        main="Cumulative Fatal Police Killings by Month in WA State: 2000-2018",
        xlab = "Month", ylab = "Cumulative Number Killed",
        xaxt = "n")
matlines(x=1:12, y=fewa.cumsum[,11:19], 
         type='l', col='grey', pch='*', lty=3)

axis(1, at=1:12, 
     labels=c('Jan','Feb','Mar','Apr','May','Jun',
              'Jul','Aug','Sep','Oct','Nov','Dec'),
     cex.axis=0.7)

matlines(x=1:12, y=apply(fewa.cumsum[1:10], 1, 'mean'),
         type='l', col='gold4', lty=3, lwd=3)
matlines(x=1:12, y=apply(fewa.cumsum[11:19], 1, 'mean'),
         type='l', col='black', lty=3, lwd=3)

matlines(x=1:thismonth, y=fewa.cumsum[1:thismonth,19],
         type='l', col='red', lty=1, lwd=2)

legend("topleft", cex=0.6,
       legend = c("Individual Years", 
                  "Average, 2010's",
                  "Average, 2000's",
                  "2018"),
       lty = c(3,3,3,1), lwd = c(1,2,2,2),
       col = c("grey","black","gold4","red"))

fewa.current <- filter(tmp.data, state == "WA" & year == 2018)
