library(ggplot2)
library(plotly)

errorTable <- NULL

errorModelNames <- c("AR", "NN","NNH","NNFA", "NNHFA")



error_metric <- function(testResults)
{
  #forecast_set ist von Datentyp matrix, muss aber numeric sein
  test_set <- as.numeric(testResults$expected)
  forecast_set <- as.numeric(testResults$result)
  
  data.frame(mse = unlist(lapply(1:length(test_set), function(i) { mse(test_set[[i]], forecast_set[[i]]) })),
    rmse = unlist(lapply(1:length(test_set), function(i) { rmse(test_set[[i]], forecast_set[[i]]) })),
    smape = unlist(lapply(1:length(test_set), function(i) { sMAPE(test_set[[i]], forecast_set[[i]]) })),
    diff = unlist(lapply(1:length(test_set), function(i) { abs(test_set[[i]] - forecast_set[[i]]) })))
}

applyMetric <- function(getTestResultsFUN, FUN)
{
  if(!errorTypCheck){
    unlist(lapply(names(data.sets), function(id) {
      testResults <- getTestResultsFUN(id)
      unlist(lapply(1:length(testResults$expected), function(i) {
        FUN(testResults$expected[[i]], testResults$result[[i]])
      }))
    }))
  }else{

      testResults <- getTestResultsFUN(data.idSelected)
      unlist(lapply(1:length(testResults$expected), function(i) {
        FUN(testResults$expected[[i]], testResults$result[[i]])
      }))

  }
}

getErrorMetric <- function(FUN)
{
  metric <- data.table(ar = applyMetric(getARTestResults, FUN))
  
  if(neuralNetwork.enableForEach)
  {
    metric$nn <- applyMetric(getNeuralNetworkTestResults, FUN)
  }
  
  if(neuralNetwork.enableForEach.hidden)
  {
    metric$nnh <- applyMetric(function(id) { getNeuralNetworkTestResults(id, hiddenLayers = TRUE) }, FUN)
  }
  
  if(neuralNetwork.enableForAll)
  {
    metric$nnfa <- applyMetric(function(id) { getNeuralNetworkTestResults(id, forAll = TRUE) }, FUN)
  }
  
  if(neuralNetwork.enableForAll.hidden)
  {
    metric$nnfah <- applyMetric(function(id) { getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE) }, FUN)
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
    line = list(color = 'rgb(200,0,0)'),
    name = "AR", boxmean = TRUE)
  
  if(neuralNetwork.enableForEach)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nn, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,200,0)'),
                name = "NN", boxmean = TRUE)
  }
  
  if(neuralNetwork.enableForEach.hidden)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnh, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,0,200)'),
                name = "NNH", boxmean = TRUE)
  }
  
  if(neuralNetwork.enableForAll)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnfa, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(200,200,0)'),
                name = "NNFA", boxmean = TRUE)
  }
    
  if(neuralNetwork.enableForAll.hidden)
  {
    p <- p %>%  add_boxplot(y = errorMetrics$nnfah, jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,200,200)'),
                name = " NNHFA")
  }
    
    
  p$elementId <- NULL
  p
}


getMeanErrorVectorFromModels <- function(errorName)
{
  comparison()
  
  errorMean = mean(errorTable[[errorName]][['ar']])
  if(neuralNetwork.enableForEach) errorMean = c(errorMean, mean(errorTable[[errorName]][['nn']]))
  if(neuralNetwork.enableForEach.hidden) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnh']]))
  if(neuralNetwork.enableForAll) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnfa']]))
  if(neuralNetwork.enableForAll.hidden) errorMean = c(errorMean, mean(errorTable[[errorName]][['nnfah']]))
  errorMean
}


getErrorMetricCompare <- function()
{
  comparison()
  
  names = c(errorModelNames[1])
  if(neuralNetwork.enableForEach) names = c(names, errorModelNames[2])
  if(neuralNetwork.enableForEach.hidden) names = c(names, errorModelNames[3])
  if(neuralNetwork.enableForAll) names = c(names, errorModelNames[4])
  if(neuralNetwork.enableForAll.hidden) names = c(names, errorModelNames[5])
  
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
  if(neuralNetwork.enableForEach)
  {
    coef <- getReducedNeuralNetworkWeights(getNeuralNetwork(id))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfe <- coef
    names <- c(names, 'NN for each')
  }
  
  # Add Auto Regression for each with hidden layers
  if(neuralNetwork.enableForEach.hidden)
  {
    coef <- getReducedNeuralNetworkWeights(getNeuralNetwork(id, TRUE))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfeh <- coef
    names <- c(names, 'NN for each hidden')
  }
  
  # Add Auto Regression for all
  if(neuralNetwork.enableForAll)
  {
    coef <- getReducedNeuralNetworkWeights(getNeuralNetwork(NULL))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfa <- coef
    names <- c(names, 'NN for all')
  }
  
  # Add Auto Regression for all with hidden layers
  if(neuralNetwork.enableForAll.hidden)
  {
    coef <- getReducedNeuralNetworkWeights(getNeuralNetwork(NULL, TRUE))[[1]][[1]][,1]
    bias <- coef[1]
    coef <- coef[-1]
    coef[length(coef)+1] <- bias
    dt$nnfah <- coef
    names <- c(names, 'NN for all hidden')
  }
  
  names(dt) <- names
  
  dt
}



getForecastComparisionPlot <- function(id) {
  data.length <- length(data.sets[[id]]$y)
  startRealData <- max(1, data.length - 2 * data.horizon + 1)
  startPredictionIndex = data.length - startRealData - data.horizon + 1
  
  # Start with original data
  prediction <- data.table(x = data.sets[[id]]$x[startRealData:data.length],
    y = data.sets[[id]]$y[startRealData:data.length])
  
  # Add Auto Regression
  prediction$ar <- append(rep(NA, data.horizon), getARTestResults(id)$result)
  prediction$ar[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Neural Network for each
  
  prediction$nnfe <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id)$result)
  prediction$nnfe[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Auto Regression for each with hidden layers
  if(neuralNetwork.enableForEach.hidden)
  {
    prediction$nnfeh <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$result)
    prediction$nnfeh[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }
  
  # Add Auto Regression for all
  if(neuralNetwork.enableForAll)
  {
    prediction$nnfa <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, forAll = TRUE)$result)
    prediction$nnfa[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }
  
  # Add Auto Regression for all with hidden layers
  if(neuralNetwork.enableForAll.hidden)
  {
    prediction$nnfah <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$result)
    prediction$nnfah[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }

  #Add RNN from RSNNS Package
  prediction$rsnnsrnn <- append(rep(NA, data.horizon),
    testRNN(trainRNN(id, neuralNetwork.hiddenLayers), id)$result)
    prediction$rsnnsrnn[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]

  #Add MLP from RSNNS Package
  prediction$rsnnsmlp <- append(rep(NA, data.horizon),
    testMLP(trainMLP(id, neuralNetwork.hiddenLayers), id)$result)
    prediction$rsnnsmlp[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Plot the data
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)')) %>%
    add_trace(y = ~ar, name = 'Auto Regression', line = list(color = 'rgb(255, 0, 0)'))
  if(neuralNetwork.enableForEach)
  {
    p <- p %>% add_trace(y = ~nnfe, name = 'Neural Network /1', line = list(color = 'rgb(255, 150, 0)'))
  }
  if(neuralNetwork.enableForEach.hidden)
  {
    p <- p %>% add_trace(y = ~nnfeh, name = 'Neural Network /1 hidden', line = list(color = 'rgb(0, 255, 255)'))
  }
  if(neuralNetwork.enableForAll)
  {
    p <- p %>% add_trace(y = ~nnfa, name = 'Neural Network /n', line = list(color = 'rgb(0, 0, 255)'))
  }
  if(neuralNetwork.enableForAll.hidden)
  {
    p <- p %>% add_trace(y = ~nnfah, name = 'Neural Network /n hidden', line = list(color = 'rgb(255, 0, 225)'))
  }
  p <- p %>% add_trace(y = ~rsnnsrnn, name = 'RSNNS rnn', line = list(color = 'rgb(150, 150, 0)'))
  p <- p %>% add_trace(y = ~rsnnsmlp, name = 'RSNNS mlp', line = list(color = 'rgb(75, 75, 0)'))
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <- function()
{
  errorTable <<- NULL
}
