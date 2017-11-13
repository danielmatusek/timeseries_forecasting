library(plotly)
library(forecast)
library(stats)

model <<- NULL

ARModel <- function(consumptionData, window, predValue)
{
  if(is.null(consumptionData)  || is.null(window) || is.null(predValue))
  {
    return(NULL)
  }
  
  predictValue <<- predValue
  spl <<- length(consumptionData) - predictValue
  trainData <<- consumptionData[1: spl]
  testData <<- consumptionData[(spl+1): length(consumptionData)]
  
  #ar = stats::arima(ts(trainData, start = 1, end = spl), order= c(window,0,0), optim.method = "Nelder-Mead")
  ar = stats::ar(ts(trainData),aic=FALSE, window, method ="burg")
  model$ar <<- ar
  model$coef <<- ar$coef
  tsPred = predict(ar,n.ahead=predictValue)
  model$forecast <<- tsPred$pred
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
    add_lines(x = ((spl+1): (spl+predictValue)), y = model$forecast, color = I("red"), name = "forecast data")
   
  p$elementId <- NULL
  p
}


getARCoef <- function()
{
  model$coef
}

error_metric_AR <- function(){
  mse <- mse(testData, model$forecast)
  rmse <- rmse(testData, model$forecast)
  smape <- sMAPE(testData, model$forecast)
  data.frame(mse = mse,rmse = rmse, smape = smape)
}


