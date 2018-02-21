library(ggplot2)
library(plotly)

###This script prepares and creates plots/data for the Comparion panel in the UI.

errorTable <- NULL #errorTable stored here

#' get the error metric for a id and specified metric
#'
#' @param id id of the time series
#' @param FUN function name of the metric, e.g. SMAPE
#' @return result of the metric
getErrorMetric <- function(id, FUN)
{
  metric <- lapply(vars$enabledModels, function(modelName) {
    testResults <- getTestResults(modelName, id)
    unlist(lapply(1:length(testResults$expected), function(i) {
      FUN(testResults$expected[[i]], testResults$predicted[[i]])
    }))
  })
  names(metric) <- vars$enabledModels
  
  metric
}

#' creates a table with mse, rmse, smape and diff metrics for the specified id
#'
#' @param id id of the time series
#' @return errorTable with errorMetrics
comparison <- function(id)
{
  if(is.null(errorTable))
  {
    errorTable$mse  <<- getErrorMetric(id, mse)
    errorTable$rmse <<- getErrorMetric(id, rmse)
    errorTable$smape <<- getErrorMetric(id, sMAPE)
    errorTable$diff <<- getErrorMetric(id, function(x, y) { abs(x - y) })
  }
  return(errorTable)
}

#' plot the error metric
#'
#' @param errorMetricName name of the error metric
#' @param id id of the time series
#' @return return the plot
getModelErrorPlot <- function(errorMetricName, id)
{
  FUN <- switch(errorMetricName,
    'smape' = { sMAPE },
    'mse' = { mse },
    'rmse' = { rmse }
  )
  
  p <- plot_ly(type = 'box')

  for (modelName in vars$enabledModels)
  {
    testResults <- getTestResults(modelName, id)
    errors <- NULL
    

    if(inherits(testResults, 'TestResults'))
    {
      errors <- unlist(lapply(1:length(testResults$expected), function(i) {
        FUN(testResults$expected[[i]], testResults$predicted[[i]])
      }))
      
      p <- p %>% add_boxplot(y = errors, line = list(color = modelColors[[modelName]]),
                             name = modelName, boxmean = TRUE)
      
    }
    
    
  }

  p$elementId <- NULL
  p
}

saveErrors <- function()
{
  filename <- paste("ModelName", "SMAPE","CPUT", '.csv', sep="")
  x <- data.frame("ModelName", "SMAPE","CPUT")
  write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";")
  
  for (modelName in vars$enabledModels)
  {
    
    
    filename <- paste(modelName, "_Error", '.csv', sep="")
    x <- data.frame("ID", "Smape", "cpuTime")
    write.table(x, file = filename, append = FALSE, row.names = FALSE, col.names = FALSE, sep=";")
    ids <- vector()
    smapes <- vector()
    cpuTime <- vector()
    for(id in names(vars$timeSeries))  
    {
      result <- getTestResults(modelName, id)
      if(!is.na(result[1]))
      {
        smape <- sMAPE(result$expected, result$predicted)
        if(!is.logical(smape[1]))
        {
          ids <- c(ids, id)
          smapes <- c(smapes, smape)
          cpu <-  getCpuTimes(modelName, id, na.rm = TRUE)
          cpuTime <- c(cpuTime, cpu)
          save(filename,id, sMAPE(result$expected, result$predicted), cpu)
        }
      }
    }
    x <- data.frame("SMAPE:",mean(smape),"CPUTIME:", mean(cpuTime))
    write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";")
    
    filename <- paste("ModelName", "SMAPE","CPUT", '.csv', sep="")
    x <- data.frame(modelName, mean(smape), mean(cpuTime))
    write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";")
  }
}



save <- function(filename, id, smape, cpuTime)
{
  x <- data.frame(id, smape, cpuTime)
  write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";")
}





#' get the mean error vector from models
#'
#' @param id id of the time series
#' @param errorName name of the error metric
#' @return mean error vector
getMeanErrorVectorFromModels <- function(id, errorName)
{
  comparison(id)
  
  unlist(lapply(vars$enabledModels, function(modelName) {
    mean(errorTable[[errorName]][[modelName]])
  }))
}

#' create a table to compare the error metrics by their value
#'
#' @param id id of the time series
#' @return data table with error metrics
getErrorMetricCompare <- function(id)
{
  comparison(id)
  
  b = 1
  data.table(NAME = vars$enabledModels, MSE = getMeanErrorVectorFromModels(id, "mse"),
             RMSE =  getMeanErrorVectorFromModels(id, "rmse"), SMAPE = getMeanErrorVectorFromModels(id, "smape"))
}

#' gets the coefficients of e.g. the ar model or the weights of the neural network to
#' compare e,g, whether they calculate the same
#'
#' @param id id of the time series
#' @return data table with coefficents
getCoef <- function(id)
{
  variables <- NULL
  for(i in 1 : (vars$options$windowSize+1))
  {
    variables = c(variables, paste(c("x", i), collapse = ""))
  }
  variables[length(variables)] <- 'Bias/Mean'
  
  dt <- data.table(Variables = variables)
  names <- c('Variables')
  
  # Add Autoregressive
  dt$arc = getModel('ar', id) # the ar model is the set of coefficients
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

#' Compares the test results of two given models using all time series.
#' The test results are said to differ´in one point if the difference of both predictions
#' exceeds the threshold value multiplied by the value span of the complete time series.
#' Supported models: ar, nnfe, nnfeh, nnfa, nnfah, jordan, elman, mlp, mlph
#'
#' @param modelName1 name of the first model
#' @param modelName2 name of the second model
#' @param threshold threshold which is set to 0.01
#' @return test results of the given models 
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
  
  diffs <- lapply(names(vars$timeSeries), function(id) {
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

#' plot the time series which were predicted and the original one to compare
#'
#' @param id id of the time series
#' @return plot of the forecasts and original data
getForecastComparisionPlot <- function(id)
{
  data.length <- length(vars$timeSeries[[id]][,1])
  startRealData <- max(1, data.length - vars$options$horizon + 1)
  startPredictionIndex = data.length - startRealData - vars$options$horizon + 1
  
  # Start with original data
  original <- data.table(y = vars$timeSeries[[id]][startRealData:data.length, 1])
  
  # Plot the data
  p <- plot_ly(original, y = ~y, text = "original", type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)'))

  for(modelName in vars$enabledModels)
  {
    testResults <- getTestResults(modelName, id)
    if (mode(testResults) != 'logical')
    {
      index <- which(availableModels == modelName)
      p <- p %>% add_trace(y = testResults$predicted, text = modelText[index], name = modelName,
        line = list(color = modelColors[[modelName]]))
    }
  }
 
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

#' resets the error table
resetComparison <- function()
{
  errorTable <<- NULL
}
