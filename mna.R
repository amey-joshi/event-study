# A quick study of mergers and acquisitions

source("gui/es.R")
get_data <- function(stock.ticker, event.date) {
  index.ticker <- "^GSPC"
  start.date <- as.Date(event.date) - 90
  end.date   <- as.Date(event.date) + 90
  
  get_closing_data(stock.name = stock.ticker,
                   index.name = index.ticker,
                   from = start.date,
                   to = end.date)
}

mna.list <- read.csv("mna.csv")
mna.list$Event <- as.character(mna.list$Event)

mna.data <- apply(mna.list, 1, function(x) get_data(x[1], x[2]))

get_stats <- function(stock.xts, index.xts, event.date) {
  event.date <- trimws(event.date)
  stock <- stock.xts[[1]]
  index <- index.xts[[1]]
  event.time <- event.time <- which(index(stock) == event.date)
  car.res <- compute_car(stock, index, event.time)
  bhar.res <- compute_bhar(stock, index, event.time)
  list(car.res, bhar.res)
}

L <- list()
j <- 1
for (i in seq_len(length(mna.data))) {
  L[[j]] <- get_stats(mna.data[[i]][1], mna.data[[i]][2], mna.list[i, 2])
  j <- j + 1
}

all.bhar.before <- c()
all.bhar.after <- c()

for (i in seq_len(length(L))) {
  all.bhar.before[i] <- L[[i]][[2]][1]
  all.bhar.after[i] <- L[[i]][[2]][2]
}

summary(all.bhar.before)
summary(all.bhar.after)

# There is no significant change in BHAR due to M&A.
wilcox.test(all.bhar.before,all.bhar.after)
t.test(all.bhar.after, all.bhar.before)
response <- c(all.bhar.before, all.bhar.after)
when <- c(rep("Before", 19), rep("After", 19))
X <- data.frame(bhar = response, when = when)
oneway.test(response ~ when, data = X)

plot(
  1:length(all.bhar.before),
  all.bhar.before,
  xlab = "Instances",
  ylab = "BHAR",
  main = "BHAR before and after",
  pch = 3,
  cex = 0.7
)
points(1:length(all.bhar.after),
       all.bhar.after,
       pch = 4,
       cex = 0.7)
legend("topleft", legend = c("Before", "After"), pch = c(3, 4), cex = 0.7)

