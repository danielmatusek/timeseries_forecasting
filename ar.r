library(plotly)
library(forecast)
library(stats)
library(data.table)

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

getTestResults.ar <- function(model, id)
{
  testSet <- cbind(getTestSet(id), 1)
  
  (testSet %*% model)[, 1]
}
