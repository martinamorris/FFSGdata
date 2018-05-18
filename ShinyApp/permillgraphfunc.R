
permillgraph <- function(state, all, capita) {
  df <- permillcalc(capita = capita)
  if(!capita){
    df <- head(df,-1)
    if (all) {
      matplot(
        2000:2017,
        t(df[, 3:20]),
        type = "l",
        col = "grey",
        xlab = "Year",
        ylab = "Total Fatal Events",
        main = "Police Killings in US by State"
      )
      
      lines(2000:2017, df[df$state_name == state, 3:20],
            col = "red")
      
      legend(
        "topright",
        legend = c(state, "United States", "Other States"),
        lty = c(1, 1),
        lwd = c(2.5, 2.5),
        col = c("red", "black", "grey"),
        text.col = "black",
        bty = "n",
        cex = 0.75
      )
      }else{
        plot(
        2000:2017,
        df[df$state_name == state, 3:20],
        type = "l",
        col = "red",
        xlab = "Year",
        ylab = "Total Fatal Events",
        main = paste("Total Police Killings in", state, sep = " ")
        )
      }
  }else{
    if (all) {
      matplot(
        2000:2017,
        t(df[, 3:20]),
        type = "l",
        col = "grey",
        xlab = "Year",
        ylab = "Total Fatal Events/Million Population",
        main = "Police Killings in US by State"
      )
      
      lines(2000:2017, df[df$state_name == "United States", 3:20],
              col = "black")
      
      lines(2000:2017, df[df$state_name == state, 3:20],
            col = "red")
      
      legend(
        "topright",
        legend = c(state, "United States", "Other States"),
        lty = c(1, 1),
        lwd = c(2.5, 2.5),
        col = c("red", "black", "grey"),
        text.col = "black",
        bty = "n",
        cex = 0.75
      )
    }else{
      plot(
        2000:2017,
        df[df$state_name == state, 3:20],
        type = "l",
        col = "red",
        xlab = "Year",
        ylab = "Total Fatal Events/Million Population",
        main = paste("Total Police Killings in", state, sep = " ")
      )
    }
  }
}
