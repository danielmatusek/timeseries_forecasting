library(ggplot2)
library(plotly)

errorTable <- NULL

errorModelNames <- c("AR", "NN","NNH","NNFA", "NNHFA")




comparison <- function()
{
  if(is.null(errorTable))
  {
    ids = names(data.sets)
    len = length(ids)
    
    errorListAR <- list()
    errorListNN <- list()
    errorListNNH <- list()
    errorListNNFA <- list()
    errorListNNHFA <- list()
    
    
    for(i in 1 : len)
    {
      id = ids[i]
      
      errorListAR[[i]] = error_metric(getARModel(id)$expected, getARModel(id)$result)
      if(neuralNetwork.enableForEach) errorListNN[[i]] = error_metric(getNeuralNetworkTestResults(id)$net.expected, getNeuralNetworkTestResults(id)$net.result)
      if(neuralNetwork.enableForEach.hidden) errorListNNH[[i]] = error_metric(getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.result)
      if(neuralNetwork.enableForAll) errorListNNFA[[i]] = error_metric(getNeuralNetworkTestResults(id, forAll = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE)$net.result)
      if(neuralNetwork.enableForAll.hidden) errorListNNHFA[[i]] =  error_metric(getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
    }
    
    errorTable <<- data.table(AR = errorListAR, NN = errorListNN, NNH = errorListNNH, NNFA = errorListNNFA, NNHFA = errorListNNHFA)
  }
  return(errorTable)
}


getErrorNameValue <- function(errorName)
{
  if(errorName == "mse")
  {
    return(1)
  }
  else if(errorName == "rmse")
  {
    return(2)
  } 
  else if(errorName == "smape")
  {
    return(3)
  }
  return(NULL)
}


getListOfErrorFromModel <-  function(modelName, errorName)
{
  error <- vector()
  for(i in 1 : length(errorTable))
  {
    eName = names(errorTable)[i]
    if(eName == modelName)
    {
      for(j in 1 : length(errorTable[[i]]))
      {
        error = c(error, errorTable[[i]][[j]][getErrorNameValue(errorName)][[1]])
      }
      return(error)
    }
  }
}





getBoxplot <- function(errorName)
{
  comparison()
  p <- plot_ly(type = "box")
  
  p <- p %>% add_boxplot(y = getListOfErrorFromModel("AR", errorName),  jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
    marker = list(color = 'rgb(7,40,89)'),
    line = list(color = 'rgb(200,0,0)'),
    name = "AR", boxmean = TRUE)
  
  if(neuralNetwork.enableForEach)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NN", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,200,0)'),
                name = "NN", boxmean = TRUE)
  }
  
  if(neuralNetwork.enableForEach.hidden)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NNH", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,0,200)'),
                name = "NNH", boxmean = TRUE)
  }
  
  if(neuralNetwork.enableForAll)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NNFA", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(200,200,0)'),
                name = "NNFA", boxmean = TRUE)
  }
    
  if(neuralNetwork.enableForAll.hidden)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NNHFA", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
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
  
  errorMean = mean(getListOfErrorFromModel(errorModelNames[1], errorName))
  if(neuralNetwork.enableForEach) errorMean = c(errorMean, mean(getListOfErrorFromModel(errorModelNames[2], errorName)))
  if(neuralNetwork.enableForEach.hidden) errorMean = c(errorMean, mean(getListOfErrorFromModel(errorModelNames[3], errorName)))
  if(neuralNetwork.enableForAll) errorMean = c(errorMean, mean(getListOfErrorFromModel(errorModelNames[4], errorName)))
  if(neuralNetwork.enableForAll.hidden) errorMean = c(errorMean, mean(getListOfErrorFromModel(errorModelNames[5], errorName)))
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
    dt$nnfe <- rev(getReducedNeuralNetworkWeights(getNeuralNetwork(id))[[1]][[1]][,1])
    names <- c(names, 'NN for each')
  }
  
  # Add Auto Regression for each with hidden layers
  if(neuralNetwork.enableForEach.hidden)
  {
    dt$nnfeh <- rev(getReducedNeuralNetworkWeights(getNeuralNetwork(id, TRUE))[[1]][[1]][,1])
    names <- c(names, 'NN for each hidden')
  }
  
  # Add Auto Regression for all
  if(neuralNetwork.enableForAll)
  {
    dt$nnfa <- rev(getReducedNeuralNetworkWeights(getNeuralNetwork(NULL))[[1]][[1]][,1])
    names <- c(names, 'NN for all')
  }
  
  # Add Auto Regression for all with hidden layers
  if(neuralNetwork.enableForAll.hidden)
  {
    dt$nnfah <- rev(getReducedNeuralNetworkWeights(getNeuralNetwork(NULL, TRUE))[[1]][[1]][,1])
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
  prediction$ar <- append(rep(NA, data.horizon), getARModel(id)$result)
  prediction$ar[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Neural Network for each
  prediction$nnfe <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id)$net.result)
  prediction$nnfe[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Auto Regression for each with hidden layers
  if(neuralNetwork.enableForEach.hidden)
  {
    prediction$nnfeh <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.result)
    prediction$nnfeh[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }
  
  # Add Auto Regression for all
  if(neuralNetwork.enableForAll)
  {
    prediction$nnfa <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, forAll = TRUE)$net.result)
    prediction$nnfa[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }
  
  # Add Auto Regression for all with hidden layers
  if(neuralNetwork.enableForAll.hidden)
  {
    prediction$nnfah <- append(rep(NA, data.horizon),
      getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
    prediction$nnfah[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  }
  
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
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <- function()
{
  errorTable <<- NULL
}
