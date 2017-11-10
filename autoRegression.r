library(plotly)
library(forecast)
library(stats)

trainedNumber = NULL;

getARModelList <- function(consumptionData,window, predictedValues)
{
  if(is.null(consumptionData)  || is.null(predictedValues))
  {
    return(NULL)
  }
  
  split = length(consumptionData) - predictedValues
  trainData = consumptionData[1: split]
  ar = stats::arima(ts(trainData,start = 1, end = split), order= c(window,0,0))
  fc = forecast(ar, h = predictedValues)
  list(ar = ar, fc = fc, actual = fc$x, fitted = fc$fitted, mean = fc$mean, coef = ar$coef)
}



getPlotlyModel <- function(aRMList, consumptionData, predictedValues)
{
  split = length(consumptionData) - predictedValues
  testConsumption =  consumptionData[(split+1) : length(consumptionData)]
  
  p <- plot_ly()%>%
    add_lines(x = (1 : split), y = aRMList$actual, color = I("blue"), name = "actual data")%>%
    add_lines(x = (1 : split), y = aRMList$fitted, color = I("black"), name = "fitted data")%>%
    add_lines(x = ((split+1): length(consumptionData)), y = aRMList$mean, color = I("red"), name = "forecast data")%>%
    add_lines(x = ((split+1): length(consumptionData)), y = testConsumption, color = I("blue"), name = "actual fc data")
  p$elementId <- NULL
  p
}


getMLE <- function(consumptionData, window, predictedValues)
{
  aRMList = getARModelList(consumptionData, window, predictedValues)
  split = length(consumptionData)  - predictedValues + 1
  testData = consumptionData[split : length(consumptionData)]
  mse = sum((testData - aRMList$mean)^2) / length(aRMList$mean)
  data.frame(mse = mse)
}

getCoef <- function(consumptionData, window, predictedValues)
{
  aRMList = getARModelList(consumptionData, window, predictedValues)
  data.frame(coef = aRMList$coef)
}



