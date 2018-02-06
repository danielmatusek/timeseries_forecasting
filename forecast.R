getForecastHW <- function(ts, h){
  m <- HoltWinters(ts)
  p <- predict(m, h, prediction.interval = TRUE)
  plot(m, p)
}

getForecastNaive <- function(ts,h){
  plot(naive(ts, h=h))
}