library(plotly)
library(forecast)
library(stats)
library(data.table)

# get the model for the autoregressive model (options in the UI determine the package which is used)
# --id: id of the time series
getModel.ar <- function(id)
{
  trainData <- tail(vars$timeSeries[[id]][, 1], -vars$options$horizon)

  if(vars$options$arModelName == 'ar')
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, vars$options$windowSize, method = "burg", demean = !vars$options$excludeBias)
    return (c(arModel$ar, arModel$x.mean))
  }
  else if (vars$options$arModelName == 'autoArima')
  {
    arModel <- auto.arima(ts(trainData), start.p = vars$options$windowSize, max.p = vars$options$windowSize, d = 0, max.q = 0)
    return (arModel$coef)
  }
}

# get the test results for the autoregressive model
# --model: the model object
# --id: id of the time series
getTestResults.ar <- function(model, id)
{
  testSet <- cbind(getTestSet(id, delay = 0, seasonality = NULL), 1)
  
  (testSet %*% model)[, 1]
}
