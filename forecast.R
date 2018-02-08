getForecastHW <- function(ts, h){
  m <- HoltWinters(ts)
  p <- predict(m, h, prediction.interval = TRUE)
  plot(m, p)
}

getForecastNaive <- function(ts,h){
  plot(naive(ts, h=h))
}

forecast <- function(id, modelName, windowSize, horizon, hiddenLayers = c(1))
{
  isForecast <<- TRUE
  vars$options$windowSize <<- windowSize
  vars$options$horizon <<- horizon
  vars$options$hiddenLayers <<- hiddenLayers
  
  resetModels(modelName)
  
  if (modelName == 'ar')
  {
    model <- getModel('ar', id)
    data <- c(tail(vars$timeSeries[[id]][, 1], windowSize), 1)
    prediction <- c()
    for (i in 1:horizon)
    {
      pred <- data %*% model
      prediction <- c(prediction, pred)
      data <- c(data[2:windowSize], pred, 1)
    }
  }
  else
  {
    prediction <- getTestResults(modelName, id)$predicted
  }
  
  isForecast <<- FALSE
  resetModels(modelName)
  
  prediction
}
