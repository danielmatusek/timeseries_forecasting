library(plotly)
library(forecast)
library(stats)

ar.models <- NULL
spl <- NULL

getARModel <- function(id, arModelName)
{

  y <- data.sets[[id]]$y
  
  if(is.null(y)  || is.null(data.windowSize) || is.null(data.horizon))
  {
    return(NULL)
  }
  
  spl <<- length(y) - data.horizon
  trainData <<- y[(1 : spl)]
  testData <<- y[-(1 : spl)]
  arModel <- NULL
  coef <- NULL
  if(arModelName == "AR")
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, data.windowSize, method = "burg", demean = FALSE)
    coef <- arModel$ar
  }
  else if (arModelName == "AutoArima")
  {
    arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
    coef <- arModel$coef[1 : (length(arModel$coef) - 1)]
  } 
  
  tsPred = predict(arModel, n.ahead = data.horizon)
  model <<- list(coef = coef, trained = trainData, result = tsPred$pred, expected = testData)
  model
}


getAllARModels <-function()
{
    ids = names(data.sets)
    for(id in 1 : length(ids))
    {
      ar.models[[id]] <<- getARModel(id, "AR")
    }
}

resetARModel <- function(id, aRModelName)
{
  getARModel(id, aRModelName)
}

getPlotlyModel <- function()
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
    add_lines(x = (1 : spl), y = model$trained, color = I("blue"), name = "Original")%>%
    add_lines(x = ((spl + 1): (spl + data.horizon)), y = model$expected, color = I("blue"), name = "Original FC")%>%
    add_lines(x = ((spl + 1): (spl + data.horizon)), y = model$result, color = I("red"), name = "Prediction")%>%
    layout(xaxis = x, yaxis = y)
   
  p$elementId <- NULL
  p
}
