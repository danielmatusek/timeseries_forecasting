#' gets the HoltWinters forecast for a time series
#'
#' @param ts time series
#' @param h horizon
#' @return Returns the plot for the HoltWinters prediction
getForecastHW <- function(ts, h){
  m <- HoltWinters(ts)
  p <- predict(m, h, prediction.interval = TRUE)
  plot(m, p)
}

#' gets the naive forecast for a time series
#'
#' @param ts time series
#' @param h horizon 
#' @return Returns the plot for the naive prediction
getForecastNaive <- function(ts,h){
  plot(naive(ts, h=h))
}

#' makes a forecast (used in the datascience cup)
#'
#' @param id id of the time series
#' @param modelName name of the model
#' @param windowSize window size
#' @param horizon horizon for the future
#' @param hiddenLayers vector for the hidden layer
#' @return prediction for the parameters
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
      if (windowSize < 2)
      {
        data <- c(pred, 1)
      }
      else
      {
        data <- c(data[2:windowSize], pred, 1)
      }
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
