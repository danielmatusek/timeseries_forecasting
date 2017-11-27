library(plotly)
library(forecast)
library(stats)
library(data.table)

autoRegressiveModels <- NULL
spl <- NULL
aRModelName <- NULL

getARModel <- function(id)
{
  #print(paste("AR", id, sep=" "))
  y <- data.sets[[id]]$y
  
  if(is.null(y)  || is.null(data.windowSize) || is.null(data.horizon))
  {
    return(NULL)
  }
  
  spl <<- length(y) - data.horizon
  trainData <<- y[(1 : spl)]
  testData <<- y[-(1 : spl)]

  if(aRModelName == "AR")
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, data.windowSize, method = "burg", demean = !neuralNetwork.excludeBias)
    coef <- arModel$ar
  }
  else if (aRModelName == "AutoArima")
  {
    #arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
    #coef <- arModel$coef
    arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
    coef <- arModel$coef
    #create output datatable
    ar_manual_result <- data.table(V0 = 0)
    #offset for forecast to use whole input data.table
    ar_offset <- length(trainData) + 1
    for(i in 1:length(testData)){
      ar_forecast_result <- 0
      for(j in 1:length(coef - 1)){
        ar_forecast_result <- ar_forecast_result + coef[j] * y[ar_offset - j]
      }
      ar_forecast_result <- ar_forecast_result + coef[length(coef)]
      ar_manual_result[i] <- ar_forecast_result
      ar_offset <- ar_offset + 1
    }
    print(ar_manual_result)
  } 
  else if (aRModelName == "ManualForecast")
  {
    #get coefficients
    
  }
  #print(coef)
  if(aRModelName == "AR"){
    tsPred <- predict(arModel, n.ahead = data.horizon)
    model <- list(coef = coef, trained = trainData, result = tsPred$pred, expected = testData)
    print(tsPred$pred)
  }
  else {
    model <- list(coef = coef, trained = trainData, result = ar_manual_result, expected = testData)
  }
  model
}


getAllARModels <-function()
{
    if(is.null(autoRegressiveModels))
    {
      ids = names(data.sets)
      for(id in  ids)
      {
        autoRegressiveModels[[id]] <<- getARModel(id)
      }
    }
  return(autoRegressiveModels)
    
}

resetARModels <- function()
{
  autoRegressiveModels <<- NULL
  getAllARModels()
}

getPlotlyModel <- function(id)
{
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  x <- list(
    title = data.names$x,
    titlefont = f
  )
  
  y <- list(
    title = data.names$y,
    titlefont = f
  )
  
  p <- plot_ly()%>%
    add_lines(x = (1 : spl), y = autoRegressiveModels[[id]]$trained, color = I("blue"), name = "Original")%>%
    add_lines(x = ((spl + 1): (spl + data.horizon)), y = autoRegressiveModels[[id]]$expected, color = I("blue"), name = "Original FC")%>%
    add_lines(x = ((spl + 1): (spl + data.horizon)), y = autoRegressiveModels[[id]]$result, color = I("red"), name = "Prediction")%>%
    layout(xaxis = x, yaxis = y)
   
  p$elementId <- NULL
  p
}
