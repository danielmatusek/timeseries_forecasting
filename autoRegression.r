library(plotly)
library(forecast)
library(stats)
library(data.table)

aRModelName <- NULL

learnARModel <- function(id)
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
  
  autoRegressiveModels[[id]] <<- arModel
}

getARModel <- function(id)
{
  if (is.null(autoRegressiveModels[[id]]))
  {
    learnARModel(id)
    
  }
  
  return (autoRegressiveModels[[id]])
}

getARCoef <- function(id)
{
  arModel <- getARModel(id)
  
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

testAR <- function(id)
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
  
  autoRegressiveTestResults[[id]] <<- structure(list(expected = expected, result = result[,1]), class = 'TestResults')
}

getARTestResults <- function(id)
{
  if (is.null(autoRegressiveTestResults[[id]]))
  {
    testAR(id)
  }
  
  return (autoRegressiveTestResults[[id]])
}

resetARModels <- function()
{
  autoRegressiveModels <<- NULL
  resetARTestResults()
}

resetARTestResults <- function()
{
  autoRegressiveTestResults <<- NULL
}
