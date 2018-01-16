library(plotly)
library(forecast)
library(stats)
library(data.table)

getModel.ar <- function(id)
{
 
  y <- vars$timeSeries[[id]]$y
    
  spl <<- length(y) - vars$options$horizon
  trainData <- y[1 : spl]

  if(vars$options$arModelName == 'ar')
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, vars$options$windowSize, method = "burg", demean = !vars$options$excludeBias)
  }
  else if (vars$options$arModelName == 'autoArima')
  {
    arModel <- auto.arima(ts(trainData), start.p = vars$options$windowSize, max.p = vars$options$windowSize, d = 0, max.q = 0)
  }
  
  return (arModel)
}

getARCoef <- function(id)
{
  arModel <- getModel('ar', id)
  
  if (inherits(arModel, 'ar'))
  {
    c(arModel$ar, arModel$x.mean)
  }
  else if (inherits(arModel, 'Arima'))
  {
    arModel$coef
  }
  else
  {
    NULL
  }
}

getTestResults.ar <- function(id)
{
  testSet <- getTestSet(id)
  testSet <- testSet[, 1 : vars$options$windowSize]
  testSet[['bias']] <- 1
  
  as.matrix(testSet) %*% getARCoef(id)
}
