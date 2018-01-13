library(ggplot2)
library(plotly)

errorTable <- NULL

getErrorMetric <- function(FUN)
{
  metric <- lapply(vars$enabledModels, function(modelName) {
    testResults <- getTestResults(modelName, data.idSelected)
    unlist(lapply(1:length(testResults$expected), function(i) {
      FUN(testResults$expected[[i]], testResults$predicted[[i]])
    }))
  })
  names(metric) <- vars$enabledModels
  
  metric
}

comparison <- function()
{
  if(is.null(errorTable))
  {
    errorTable$mse <<- getErrorMetric(mse)
    errorTable$rmse <<- getErrorMetric(rmse)
    errorTable$smape <<- getErrorMetric(sMAPE)
    errorTable$diff <<- getErrorMetric(function(x, y) { abs(x - y) })
  }
  return(errorTable)
}





getBoxplot <- function(errorName)
{
  comparison()
  errorMetrics <- errorTable[[errorName]]
  p <- plot_ly(type = "box")
  
  for (modelName in vars$enabledModels)
  {
    p <- p %>%  add_boxplot(y = errorMetrics[[modelName]], line = list(color = modelColors[[modelName]]),
      name = modelName, boxmean = TRUE)
  }

  p$elementId <- NULL
  p
}


getMeanErrorVectorFromModels <- function(errorName)
{
  comparison()
  
  unlist(lapply(vars$enabledModels, function(modelName) {
    mean(errorTable[[errorName]][[modelName]])
  }))
}


getErrorMetricCompare <- function()
{
  comparison()
  
  data.table(NAME = vars$enabledModels, MSE = getMeanErrorVectorFromModels("mse"), RMSE =  getMeanErrorVectorFromModels("rmse"), SMAPE = getMeanErrorVectorFromModels("smape"))
}

getCoef <- function(id)
{
  variables <- NULL
  for(i in 1 : (data.windowSize+1))
  {
    variables = c(variables, paste(c("x", i), collapse = ""))
  }
  variables[length(variables)] <- 'Bias/Mean'
  
  dt <- data.table(Variables = variables)
  names <- c('Variables')
  
  # Add Autoregressive
  dt$arc = getARCoef(id)
  names <- c(names, 'AutoRegressive')
  
  
  # Add Auto Regression for each
  if('nnfe' %in% vars$enabledModels)
  {
    coef <- getReducedNeuralNetworkWeights(getModel('nnfe', id))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfe <- coef
    names <- c(names, 'NN for each')
  }
  
  # Add Auto Regression for each with hidden layers
  if('nnfeh' %in% vars$enabledModels)
  {
    coef <- getReducedNeuralNetworkWeights(getModel('nnfeh', id))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfeh <- coef
    names <- c(names, 'NN for each hidden')
  }
  
  # Add Auto Regression for all
  if('nnfa' %in% vars$enabledModels)
  {
    coef <- getReducedNeuralNetworkWeights(getModel('nnfa'))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfa <- coef
    names <- c(names, 'NN for all')
  }
  
  # Add Auto Regression for all with hidden layers
  if('nnfah' %in% vars$enabledModels)
  {
    coef <- getReducedNeuralNetworkWeights(getModel('nnfah'))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfah <- coef
    names <- c(names, 'NN for all hidden')
  }
  

  names(dt) <- names
  
  dt
}

# Compares the test results of two given models using all time series.
# The test results are said to differ´in one point if the difference of both predictions
# exceeds the threshold value multiplied by the value span of the complete time series.
# Supported models: ar, nnfe, nnfeh, nnfa, nnfah, jordan, elman, mlp, mlph
compareModels <- function(modelName1, modelName2, threshold = 0.01)
{
  if (modelName1 == modelName2)
  {
    stop('Cannot compare one model with itself.')
  }
  
  if (!modelName1 %in% availableModels)
  {
    stop(paste0("Model '", modelName1, "' unknown"))
  }
  if (!modelName2 %in% availableModels)
  {
    stop(paste0("Model '", modelName2, "' unknown"))
  }
  
  diffs <- lapply(names(data.sets), function(id) {
    tryCatch({
      predicted1 <- getTestResults(modelName1, id)$predicted
      if (is.atomic(predicted1))
      {
        list(id = id, diff = -1)
      }
      predicted2 <- getTestResults(modelName2, id)$predicted
      if (is.atomic(predicted2))
      {
        list(id = id, diff = -1)
      }
      valueSpan <- getTimeSeriesValueSpan(id)
      
      diffRelative <- abs((predicted1 - predicted2) / valueSpan)
      
      list(id = id, diff = sum(diffRelative > threshold))
    },
    error = function(e) {
      list(id = id, diff = -1)
    })
  })
  
  # represent as a data.table, group and order by diff
  dt <- rbindlist(diffs)
  dt <- dt[, .(ids = list(id)), by = diff]
  setorder(dt, diff)
  
  dt
}



getForecastComparisionPlot <- function(id) {
  data.length <- length(data.sets[[id]]$y)
  startRealData <- max(1, data.length - 2 * data.horizon + 1)
  startPredictionIndex = data.length - startRealData - data.horizon + 1
  
  # Start with original data
  original <- data.table(x = data.sets[[id]]$x[startRealData:data.length], y = data.sets[[id]]$y[startRealData:data.length])
  
  # Plot the data
  p <- plot_ly(original, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)'))
  
  for(modelName in vars$enabledModels)
  {
    prediction <- append(rep(NA, data.horizon), getTestResults(modelName, id)$predicted)
    prediction[[startPredictionIndex]] <- original$y[[startPredictionIndex]]
    p <- p %>% add_trace(y = prediction, name = modelName, line = list(color = modelColors[[modelName]]))
  }
 
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <- function()
{
  errorTable <<- NULL
}
