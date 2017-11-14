library(plotly)
library(forecast)
library(stats)

model <- NULL

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
  
  ar = stats::ar(ts(trainData),aic = FALSE, window, method = "burg")
  tsPred = predict(ar, n.ahead = predictValue)
  model <<- list(coef = ar$ar, result = tsPred$pred, expected = testData)
  model
}



getPlotlyModel <- function()
{
  if(is.null(model))
  {
    return(NULL)
  }
  
  p <- plot_ly()%>%
    add_lines(x = (1 : spl), y = trainData, color = I("blue"), name = "train data")%>%
    add_lines(x = ((spl+1): (spl+predictValue)), y = testData, color = I("blue"), name = "test data")%>%
    add_lines(x = ((spl+1): (spl+predictValue)), y = model$result, color = I("red"), name = "forecast data")
   
  p$elementId <- NULL
  p
}





plotACF <- function(consumptionData){
  data_acf <- acf(consumptionData)
  data_acf
}

plotPACF <- function(consumptionData){
  data_pacf <- pacf(consumptionData)
  data_pacf
}


