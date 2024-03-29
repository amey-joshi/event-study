library(lattice)
library(quantmod)

#' A function that plots scaled stock price and its index.
#'
#' @param stock.prices CSV file with stock prices
#' @param index.prices CSV file with index
#' @param stock.name   Name of the stock
#' @param index.name   Name of the index
#' @param event.time   At that time the even occured
#'
#' @return Nothing
#' @export Nothing
#'
#' @examples compare_plot("./data/VOW.DE.csv", "./data/DAX.csv", "Volkswagen", "DAX", 46)
#'
compare_plot <-
  function(stock.prices, 
           index.prices,
           stock.name,
           index.name,
           event.time) {
    # The closing prices are in columns 4 and 10 of the merged xts object.
    stock.index <- merge.xts(stock.prices, index.prices)[, c(4, 10)]
    colnames(stock.index) <- c("Close.Stock", "Close.Index")
    stock.index$scaled.stock <- scale(stock.index$Close.Stock)
    stock.index$scaled.index <- scale(stock.index$Close.Index)
    stock.index$sno <- seq(from = 1,
                           to = nrow(stock.index),
                           by = 1)
    xyplot(
      scaled.stock + scaled.index ~ sno,
      data = as.data.frame(stock.index),
      type = "l",
      col = c(1, 2),
      xlab = "Time",
      ylab = "Scaled closing price",
      key = list(text = list(c(
        stock.name, index.name
      )),
      lines = list(col = c(1, 2))),
      panel = function(x, ...) {
        panel.xyplot(x, ...)
        panel.abline(v = event.time, lty = 2)
      }
    )
  }

#' Get closing prices from Yahoo Finance
#'
#' @param stock.name stock symbol
#' @param index.name index symbol
#' @param from date in the format "YYYY-MM-DD"
#' @param to date in the format "YYYY-MM-DD"
#'
#' @return a list of two xts objects, one with stock prices and other with 
#'         index prices
#' @export Nothing
#'
#' @examples X <- get_closing_data("GE", "^GSPC", "2019-06-17", "2019-10-17")
#'
get_closing_data <- function(stock.name, index.name, from, to) {
  stock <- getSymbols(stock.name, from = from, to = to, auto.assign = FALSE)
  index <- getSymbols(trimws(index.name), from = from, to = to, auto.assign = FALSE)
  list(stock, index)
}

#' Compute the Buy-and-Hold-Abnormal-Returns.
#'
#' @param stock 
#' @param index
#' @param event.time
#'
#' @return Nothing
#' @export Nothing
#'
#' @examples
compute_bhar <- function(stock, index, event.time) {
  WINDOW_SIZE <- 5
  start.time <- event.time - WINDOW_SIZE
  end.time <- event.time + WINDOW_SIZE
  # Get cumulative returns for the stock
  stock.before <-
    diff(stock[start.time:event.time, c(4)], arithmetic = FALSE)
  stock.after <-
    diff(stock[event.time:end.time, c(4)], arithmetic = FALSE)
  # Get cumulative returns for the index
  index.before <-
    diff(index[start.time:event.time, c(4)], arithmetic = FALSE)
  index.after <-
    diff(index[event.time:end.time, c(4)], arithmetic = FALSE)
  # Compute BHAR
  bhar.before <-
    prod(stock.before, na.rm = TRUE) - prod(index.before, na.rm = TRUE)
  bhar.after <-
    prod(stock.after, na.rm = TRUE) - prod(index.after, na.rm = TRUE)
  
  cat(paste("BHAR before:", round(bhar.before, 4), "\n"))
  cat(paste("BHAR after:", round(bhar.after, 4), "\n"))
  
  c(round(bhar.before, 4), 
    round(bhar.after, 4))
}

#' Compute abnormal returns statistics. Report if there are significant changes.
#'
#' @param stock 
#' @param index 
#' @param event.time 
#'
#' @return Nothing
#' @export Nothing
#'
#' @examples
compute_car <- function(stock, index, event.time) {
  WINDOW_SIZE <- 10
  start.time <- event.time - WINDOW_SIZE
  end.time <- event.time + WINDOW_SIZE
  
  stock.ret <- dailyReturn(stock[, 4])
  index.ret <- dailyReturn(index[, 4])
  
  model.1 <- lm(stock.ret[1:start.time] ~ index.ret[1:start.time], na.action = na.omit)
  alpha <- model.1$coefficients[1]
  beta  <- model.1$coefficients[2]
  sigma <- sd(model.1$residuals)
  
  est.stock <- alpha + beta * index.ret[event.time:end.time]
  res.stock <- est.stock - stock.ret[event.time:end.time]
  
  car <- sum(res.stock)
  
  res.stock.t <- res.stock/sigma
  car.t <- car/(sqrt(WINDOW_SIZE)*sigma)
  
  df.t <- event.time - 1 - 2 # 2 parameters are estimated.
  res.stock.sig <- abs(res.stock.t) >= qt(0.95, df.t)
  car.sig <- abs(car.t) >= qt(0.95, df.t)
  
  result.1 <- ifelse(any(res.stock.sig), "YES", "NO")
  cat(paste("\nSignificant change in AR", result.1, "\n"))
  result.2 <- ifelse(any(res.stock.sig), "YES", "NO")
  cat(paste("Significant change in CAR", result.2))
  
  c(result.1, result.2)
}
