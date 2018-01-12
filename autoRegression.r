library(plotly)
library(forecast)
library(stats)
library(data.table)

aRModelName <- NULL

getModel.ar <- function(id)
{
  print(paste('train ar for id', id))

  y <- data.sets[[id]]$y
  if(neuralNetwork.inputDifference) y <- diff(data.sets[[id]]$y)
    
  spl <<- length(y) - data.horizon
  trainData <- y[(1 : spl)]

  if(aRModelName == "AR" || aRModelName == "ManualAR")
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, data.windowSize, method = "burg", demean = !neuralNetwork.excludeBias)
  }
  else if (aRModelName == "AutoArima" || aRModelName == "ManualAutoArima")
  {
    arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
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
  expected <- testSet[['xt0']]
  

  testSet[['xt0']] <- NULL
  testSet[['bias']] <- 1
  result <- as.matrix(testSet) %*% getARCoef(id)
  
  if(neuralNetwork.inputDifference)
  {
    result <-  setOffsetToResultSet(id, result)
    expected <- getOrgiginalTestSet(id)
  }
  
  structure(list(expected = expected, result = result[,1]), class = 'TestResults')
}
