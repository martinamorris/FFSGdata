#source("Analysis/Tables/permillcalculation.R")

permillgraph <- function(state, all){
  
  if(all){
    matplot(2000:2017, t(fepermill[,3:20]), type="l", 
            col= "grey", xlab = "Year", ylab = "Total Killings/Million", main = "Total Police Killings in US by State")
  
  
    lines(2000:2017, fepermill[fepermill$state_name=="United States",3:20], 
            col="black")
  
    lines(2000:2017, fepermill[fepermill$state_name==state,3:20], 
            col= "red")
  
    legend("topright", legend = c(state, "United States", "Other States"), lty = c(1,1), lwd = c(2.5,2.5), 
            col = c("red", "black", "grey"), text.col = "black", bty = "n", cex = 0.75)
  }else{
    plot(2000:2017, fepermill[fepermill$state_name==state,3:20], type="l", col= "red", xlab = "Year", ylab = "Total Killings/Million", main = paste("Total Police Killings in",state,sep = " "))
  }
}

