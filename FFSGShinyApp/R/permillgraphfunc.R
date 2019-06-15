source("R/permillcalculation.R")

#' perMillGraph
#'
#' Creates a graph of fatal encounters by state
#'
#' @param state a state name, the state that is graphed or highlighted
#' @param all a boolean, if TRUE graphs state over the data of other states, if FALSE only graphs the given state
#' @param capita a boolean, if TRUE evaluates cases per million population of each state, if FALSE calculates total cases
#'
#' @seealso \code{\link{permillcalc}} and \code{\link{permilltable}}
#'
#' @export

## @knitr permillgraph

permillgraph <- function(state, all, capita) {
  df <- permillcalc(capita = capita)
  if(!capita){
    df <- head(df,-1)
    if(state == "United States"){
      if (all) {
        matplot(
          2000:(as.integer(format(Sys.Date(), "%Y"))-2),
          t(df[, 3:20]),
          type = "l",
          col = "grey",
          xlab = "Year",
          ylab = "Total Fatal Events",
          main = "Police Killings in US by State"
        )

        lines(2000:(as.integer(format(Sys.Date(), "%Y"))-2), df[df$state_name == state, 3:20],
              col = "red")
        legend(
          "topright",
          legend = c(state, "States"),
          lty = c(1, 1),
          lwd = c(2.5, 2.5),
          col = c("red", "grey"),
          text.col = "black",
          bty = "n",
          cex = 0.75
        )
      }else {
        plot(
          2000:(as.integer(format(Sys.Date(), "%Y"))-2),
          df[df$state_name == state, 3:20],
          type = "l",
          col = "red",
          xlab = "Year",
          ylab = "Total Fatal Events",
          main = paste("Total Police Killings in", state, sep = " ")
        )
      }
    }
    else {
      if (all) {
        matplot(
          2000:(as.integer(format(Sys.Date(), "%Y"))-2),
          t(df[, 3:20]),
          type = "l",
          col = "grey",
          xlab = "Year",
          ylab = "Total Fatal Events",
          main = "Police Killings in US by State"
        )

        lines(2000:(as.integer(format(Sys.Date(), "%Y"))-2), df[df$state_name == state, 3:20],
              col = "red")
        legend(
          "topright",
          legend = c(state, "Other States"),
          lty = c(1, 1),
          lwd = c(2.5, 2.5),
          col = c("red", "black", "grey"),
          text.col = "black",
          bty = "n",
          cex = 0.75
        )
        }else{
          plot(
            2000:(as.integer(format(Sys.Date(), "%Y"))-2),
          df[df$state_name == state, 3:20],
          type = "l",
          col = "red",
          xlab = "Year",
          ylab = "Total Fatal Events",
          main = paste("Total Police Killings in", state, sep = " ")
          )
        }
    }
  }else{
    if (all) {
      matplot(
        2000:(as.integer(format(Sys.Date(), "%Y"))-2),
        t(df[, 3:20]),
        type = "l",
        col = "grey",
        xlab = "Year",
        ylab = "Total Fatal Events/Million Population",
        main = "Police Killings in US by State"
      )

      lines(2000:(as.integer(format(Sys.Date(), "%Y"))-2), df[df$state_name == "United States", 3:20],
              col = "black")

      lines(2000:(as.integer(format(Sys.Date(), "%Y"))-2), df[df$state_name == state, 3:20],
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
        2000:(as.integer(format(Sys.Date(), "%Y"))-2),
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
