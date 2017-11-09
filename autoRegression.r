library(plotly)
library(forecast)
library(stats)

trainedNumber = NULL;

getARModelList <- function(consumptionData, window, predictedValues)
{
  if(is.null(consumptionData) || is.null(window) || is.null(predictedValues))
  {
    return(NULL)
  }
  
  split = length(consumptionData) - predictedValues
  trainData = consumptionData[1: split]
  ar = arima(ts(trainData,start = 1, end = split), order= c(window,0,0))
  fc = forecast(ar, h = predictedValues)
  list(ar = ar, fc = fc, actual = fc$x, fitted = fc$fitted, mean = fc$mean)
}



getPlotlyModel <- function(aRMList, consumptionData, predictedValues)
{
  split = length(consumptionData) - predictedValues
  testConsumption =  consumptionData[(split+1) : length(consumptionData)]
  
  p <- plot_ly()%>%
    add_lines(x = (1 : split), y = aRMList$actual, color = I("blue"), name = "actual data")%>%
    add_lines(x = (1 : split), y = aRMList$fitted, color = I("black"), name = "fitted data")%>%
    add_lines(x = ((split+1): length(consumptionData)), y = aRMList$mean, color = I("red"), name = "forecast data")%>%
    add_lines(x = ((split+1): length(consumptionData)), y = testConsumption, color = I("blue"), name = "actual data")
  p$elementId <- NULL
  p
}



