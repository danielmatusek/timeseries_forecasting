library(plotly)
library(forecast)
library(stats)
library(data.table)

getModel.ar <- function(id)
{
  y <- vars$timeSeries[[id]]$y
  trainData <- y[1 : (length(y) - vars$options$horizon)]

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
  testSet <- getTestSet(id)
  testSet <- testSet[, 1 : vars$options$windowSize]
  testSet[['bias']] <- 1
  
  (as.matrix(testSet) %*% model)[, 1]
}
