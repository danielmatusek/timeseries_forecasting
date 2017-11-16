library(plotly)
library(forecast)
library(stats)

model <- NULL
arima.foreach <- NULL

getARModel <- function(y, window, predValue)
{
  if(is.null(y)  || is.null(window) || is.null(predValue))
  {
    return(NULL)
  }
  
  predictValue <<- predValue
  spl <<- length(y) - predictValue
  trainData <<- y[1: spl]
  testData <<- y[(spl+1): length(y)]
  
  ar = stats::ar(ts(trainData), aic = FALSE, window, method = "burg")
  tsPred = predict(ar, n.ahead = predictValue)
  model <<- list(coef = ar$ar, result = tsPred$pred, expected = testData)
  model
}

getAllARModels <-function(window, predValue)
{
  
}

getPlotlyModel <- function()
{
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  x <- list(
    title = data.names$x,
    titlefont = f
  )
  y <- list(
    title = data.names$y,
    titlefont = f
  )
  
  p <- plot_ly()%>%
    add_lines(x = (1 : spl), y = trainData, color = I("blue"), name = "Original")%>%
    add_lines(x = ((spl+1): (spl+predictValue)), y = testData, color = I("blue"), name = "Original FC")%>%
    add_lines(x = ((spl+1): (spl+predictValue)), y = model$result, color = I("red"), name = "Prediction")%>%
    layout(xaxis = x, yaxis = y)
   
  p$elementId <- NULL
  p
}





plotACF <- function(consumptionData){
  data_acf <- acf(consumptionData, main = "ACF")
  data_acf
}

plotPACF <- function(consumptionData){
  data_pacf <- pacf(consumptionData, main = "PACF")
  data_pacf
}


