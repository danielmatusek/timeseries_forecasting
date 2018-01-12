library(ggplot2)
library(plotly)

errorTable <- NULL

errorModelNames <- c("AR", "NN","NNH","NNFA", "NNHFA","ELMAN","MLP","MLPH","JORDAN")


error_metric <- function(testResults)
{
  #forecast_set ist von Datentyp matrix, muss aber numeric sein
  test_set <- as.numeric(testResults$expected)
  forecast_set <- as.numeric(testResults$predicted)
  
  data.frame(mse = unlist(lapply(1:length(test_set), function(i) { mse(test_set[[i]], forecast_set[[i]]) })),
    rmse = unlist(lapply(1:length(test_set), function(i) { rmse(test_set[[i]], forecast_set[[i]]) })),
    smape = unlist(lapply(1:length(test_set), function(i) { sMAPE(test_set[[i]], forecast_set[[i]]) })),
    diff = unlist(lapply(1:length(test_set), function(i) { abs(test_set[[i]] - forecast_set[[i]]) })))
}

applyMetric <- function(getTestResultsFUN, FUN)
{
  #if(!errorTypCheck){
  #  unlist(lapply(names(data.sets), function(id) {
  #    testResults <- getTestResultsFUN(id)
  #    unlist(lapply(1:length(testResults$expected), function(i) {
  #      FUN(testResults$expected[[i]], testResults$result[[i]])
  #    }))
  #  }))
  #}else{
      testResults <- getTestResultsFUN(data.idSelected)
      unlist(lapply(1:length(testResults$expected), function(i) {
        FUN(testResults$expected[[i]], testResults$predicted[[i]])
      }))
  #}
}

getErrorMetric <- function(FUN)
{
  metric <- data.table(ar = applyMetric(function(id) { getTestResults('ar', id) }, FUN))
  
  if('nnfe' %in% vars$enabledModels)
  {
    metric$nn <- applyMetric(function(id) { getTestResults('nnfe', id) }, FUN)
  }
  
  if('nnfeh' %in% vars$enabledModels)
  {
    metric$nnh <- applyMetric(function(id) { getTestResults('nnfeh', id) }, FUN)
  }
  
  if('nnfa' %in% vars$enabledModels)
  {
    metric$nnfa <- applyMetric(function(id) { getTestResults('nnfa', id) }, FUN)
  }
  
  if('nnfah' %in% vars$enabledModels)
  {
    metric$nnfah <- applyMetric(function(id) { getTestResults('nnfah', id) }, FUN)
  }
  if(rsnns.rnn)
  {
    metric$rnn <- applyMetric(function(id) { testRNN(trainRNN(id, neuralNetwork.hiddenLayers), id) }, FUN)
  }
  if(rsnns.mlp)
  {
    metric$mlp <- applyMetric(function(id) { testMLP(trainMLP(id, hiddenLayers = FALSE), id) }, FUN)
  }
  if(rsnns.mlph)
  {
    metric$mlph <- applyMetric(function(id) { testMLP(trainMLP(id, hiddenLayers = TRUE), id) }, FUN)
  }
  if(rsnns.jordan)
  {
    metric$jordan <- applyMetric(function(id) { testJordan(trainJordan(id, neuralNetwork.hiddenLayers), id) }, FUN)
  }
  
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
  
  p <- p %>% add_boxplot(y = errorMetrics$ar,  jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
    marker = list(color = 'rgb(7,40,89)'),
    line = list(color = 'rgb(193,5,52)'),
    name = "AR", boxmean = TRUE)
  
  if('nnfe' %in% vars$enabledModels)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nn, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,0,255)'),
                name = "NN", boxmean = TRUE)
  }
  
  if('nnfeh' %in% vars$enabledModels)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnh, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0, 255, 255)'),
                name = "NNH", boxmean = TRUE)
  }
  
  if('nnfa' %in% vars$enabledModels)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnfa, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(255, 0, 128)'),
                name = "NNFA", boxmean = TRUE)
  }
    
  if('nnfah' %in% vars$enabledModels)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnfah, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(128, 0, 128)'),
                name = "NNHFA")
  }
  
  if(rsnns.rnn)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$rnn, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                            marker = list(color = 'rgb(7,40,89)'),
                            line = list(color = 'rgb(255, 127, 0)'),
                            name = "ELMAN", boxmean = TRUE)    
  }
  if(rsnns.mlp)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$mlp, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                            marker = list(color = 'rgb(7,40,89)'),
                            line = list(color = 'rgb(255, 0, 0)'),
                            name = "MLP", boxmean = TRUE)    
  }
  if(rsnns.mlph)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$mlph, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                            marker = list(color = 'rgb(7,40,89)'),
                            line = list(color = 'rgb(0,96,0)'),
                            name = "MLPH", boxmean = TRUE)    
  }
  if(rsnns.jordan)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$jordan, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                            marker = list(color = 'rgb(7,40,89)'),
                            line = list(color = 'rgb(0, 255, 128)'),
                            name = "JORDAN", boxmean = TRUE)    
  }    
    
  p$elementId <- NULL
  p
}


getMeanErrorVectorFromModels <- function(errorName)
{
  comparison()
  
  errorMean = mean(errorTable[[errorName]][['ar']])
  if('nnfe' %in% vars$enabledModels) errorMean = c(errorMean, mean(errorTable[[errorName]][['nn']]))
  if('nnfeh' %in% vars$enabledModels) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnh']]))
  if('nnfa' %in% vars$enabledModels) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnfa']]))
  if('nnfah' %in% vars$enabledModels) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnfah']]))
  #für RSNNS auch noch
  if(rsnns.rnn) errorMean = c(errorMean, mean(errorTable[[errorName]][['rnn']]))#names = c(names, errorModelNames[6])
  if(rsnns.mlp) errorMean = c(errorMean, mean(errorTable[[errorName]][['mlp']]))
  if(rsnns.mlph) errorMean = c(errorMean, mean(errorTable[[errorName]][['mlph']]))
  if(rsnns.jordan) errorMean = c(errorMean, mean(errorTable[[errorName]][['jordan']]))
  errorMean
}


getErrorMetricCompare <- function()
{
  comparison()
  
  names = c(errorModelNames[1])
  if('nnfe' %in% vars$enabledModels) names = c(names, errorModelNames[2])
  if('nnfeh' %in% vars$enabledModels) names = c(names, errorModelNames[3])
  if('nnfa' %in% vars$enabledModels) names = c(names, errorModelNames[4])
  if('nnfah' %in% vars$enabledModels) names = c(names, errorModelNames[5])
  if(rsnns.rnn) names = c(names, errorModelNames[6])
  if(rsnns.mlp) names = c(names, errorModelNames[7])
  if(rsnns.mlph) names = c(names, errorModelNames[8])
  if(rsnns.jordan) names = c(names, errorModelNames[9])
  data.table(NAME = names, MSE = getMeanErrorVectorFromModels("mse"), RMSE =  getMeanErrorVectorFromModels("rmse"), SMAPE = getMeanErrorVectorFromModels("smape"))
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
  
  getTestResultsFUN <- function(modelName) {
    switch (modelName,
      ar = { function(id) { getTestResults('ar', id) } },
      nnfe = { function(id) { getTestResults('nnfe', id) } },
      nnfeh = { function(id) { getTestResults('nnfeh', id) } },
      nnfa = { function(id) { getTestResults('nnfa', id) } },
      nnfah = { function(id) { getTestResults('nnfah', id) } },
      jordan = { getJordanTestResults },
      elman = { getElmanTestResults },
      mlp = { getMLPTestResults },
      mlph = { function(id) { getMLPTestResults(id, hiddenLayers = TRUE) } },
      { NULL }
    )
  }
  
  getTestResults1 <- getTestResultsFUN(modelName1)
  getTestResults2 <- getTestResultsFUN(modelName2)
  
  if (is.null(getTestResults1))
  {
    stop(paste0("Model '", modelName1, "' unknown"))
  }
  if (is.null(getTestResults2))
  {
    stop(paste0("Model '", modelName2, "' unknown"))
  }
  
  diffs <- lapply(names(data.sets), function(id) {
    tryCatch({
      predicted1 <- getTestResults1(id)$predicted
      if (is.atomic(predicted1))
      {
        list(id = id, diff = -1)
      }
      predicted2 <- getTestResults2(id)$predicted
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
  prediction <- data.table(x = data.sets[[id]]$x[startRealData:data.length],
    y = data.sets[[id]]$y[startRealData:data.length])
  
  #lapply(vars$enabledModels, function(modelName) {
  for (modelName in vars$enabledModels)
  {
    prediction[[modelName]] <- append(rep(NA, data.horizon), getTestResults(modelName, id)$predicted)
    prediction[[modelName]][[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }

  #Add RNN from RSNNS Package
  if(rsnns.rnn)
  {
    prediction$rsnnsrnn <- append(rep(NA, data.horizon),
      testRNN(trainRNN(id, neuralNetwork.hiddenLayers), id)$predicted)
      prediction$rsnnsrnn[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]    
  }

  #Add MLP with hidden Layer from RSNNS Package
  if(rsnns.mlph){
    prediction$rsnnsmlp <- append(rep(NA, data.horizon),
      testMLP(trainMLP(id, hiddenLayers = TRUE), id)$predicted)
      prediction$rsnnsmlp[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]    
  }

  #Add MLP without hidden Layer from RSNNS Package

  if(rsnns.mlp){
    prediction$rsnnsmlp_nhl <- append(rep(NA, data.horizon),
      testMLP(trainMLP(id, hiddenLayers = FALSE), id)$predicted)
      prediction$rrsnnsmlp_nhl[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]    
  }

  #Add Jordan Network from RSNNS Package
  if(rsnns.jordan){
    prediction$rsnnsjordan <- append(rep(NA, data.horizon),
      testJordan(trainJordan(id, neuralNetwork.hiddenLayers), id)$predicted)
      prediction$rsnnsjordan[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]    
  }

  
  # Plot the data
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)')) %>%
    add_trace(y = ~ar, name = 'Auto Regression', line = list(color = 'rgb(193,5,52)'))
  if('nnfe' %in% vars$enabledModels)
  {
    p <- p %>% add_trace(y = ~nnfe, name = 'Neural Network /1', line = list(color = 'rgb(0,0,255)'))
  }
  if('nnfeh' %in% vars$enabledModels)
  {
    p <- p %>% add_trace(y = ~nnfeh, name = 'Neural Network /1 hidden', line = list(color = 'rgb(0, 255, 255)'))
  }
  if('nnfa' %in% vars$enabledModels)
  {
    p <- p %>% add_trace(y = ~nnfa, name = 'Neural Network /n', line = list(color = 'rgb(255, 0, 128)'))
  }
  if('nnfah' %in% vars$enabledModels)
  {
    p <- p %>% add_trace(y = ~nnfah, name = 'Neural Network /n hidden', line = list(color = 'rgb(128, 0, 128)'))
  }
  if(rsnns.rnn)
  {
    p <- p %>% add_trace(y = ~rsnnsrnn, name = 'RSNNS elman', line = list(color = 'rgb(255, 127, 0)'))
  }
  if(rsnns.mlph)
  {
    p <- p %>% add_trace(y = ~rsnnsmlp, name = 'RSNNS mlp', line = list(color = 'rgb(0,96,0)'))    
  }

  if(rsnns.mlp)
  {
    p <- p %>% add_trace(y = ~rsnnsmlp_nhl, name = 'RSNNS mlp without hidden', line = list(color = 'rgb(255, 0, 0)'))
  }
  
  if(rsnns.jordan){
    p <- p %>% add_trace(y = ~rsnnsjordan, name = 'RSNNS jordan', line = list(color = 'rgb(0, 255, 128)'))
  }
 
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <- function()
{
  errorTable <<- NULL
}
