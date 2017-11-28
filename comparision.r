library(ggplot2)
library(plotly)
source('autoRegression.r')
source('neuralNetwork.r')
source('global.r')

errorTable <- NULL

errorModelNames <- c("AR", "NN","NNH","NNFA", "NNHFA")
#lockBinding("errorModelNames", globalenv())

choosenModelNames <<- NULL

getkInputNNTypes <- function(types)
{
  i = 1
  
  isChecked = vector()
  NNtypeVector = c("AR")
  
  for(type in types)
  {
    if(type == "forecast_one")
    {
      NNtypeVector <- c(NNtypeVector, "NN")
    }
    else if(type == "forecast_one_hidden")
    {
      NNtypeVector <- c(NNtypeVector, "NNH")
    }
    else if(type == "forecast_all")
    {
      NNtypeVector <- c(NNtypeVector, "NNFA")
    }
    else if(type == "forecast_all_hidden")
    {
      NNtypeVector <- c(NNtypeVector, "NNHFA")
    }
  }
  
  for(i in 1 : length(errorModelNames))
  {
    isChecked = c(isChecked, errorModelNames[i] %in% NNtypeVector)
  }
  return(isChecked)
}




comparison <- function()
{
  
  if(is.null(errorTable))
  {
    choosenModelNames <<- getkInputNNTypes(neuralNetwork.type)
    
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
      if(choosenModelNames[1] == TRUE) errorListAR[[i]] = error_metric(autoRegressiveModels[[id]]$expected, autoRegressiveModels[[id]]$result)
      if(choosenModelNames[2] == TRUE) errorListNN[[i]] = error_metric(getNeuralNetworkTestResults(id)$net.expected, getNeuralNetworkTestResults(id)$net.result)
      if(choosenModelNames[3] == TRUE) errorListNNH[[i]] = error_metric(getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.result)
      if(choosenModelNames[4] == TRUE) errorListNNFA[[i]] = error_metric(getNeuralNetworkTestResults(id, forAll = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE)$net.result)
      if(choosenModelNames[5] == TRUE) errorListNNHFA[[i]] =  error_metric(getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
    }
    
    errorTable <<- data.table(AR = errorListAR, NN = errorListNN, NNH = errorListNNH, NNFA = errorListNNFA, NNHFA = errorListNNHFA)
    return(errorTable)
  }
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
    
  if(choosenModelNames[1] == TRUE) 
  {
   
    p <- p %>% add_boxplot(y = getListOfErrorFromModel("AR", errorName),  jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
              marker = list(color = 'rgb(7,40,89)'),
              line = list(color = 'rgb(200,0,0)'),
              name = "AR", boxmean = TRUE)
  }
  
  if(choosenModelNames[2] == TRUE)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NN", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,200,0)'),
                name = "NN", boxmean = TRUE)
  }
  
  if(choosenModelNames[3] == TRUE)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NNH", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,0,200)'),
                name = "NNH", boxmean = TRUE)
  }
  
  if(choosenModelNames[4] == TRUE)
  {
    p <- p %>%  add_boxplot(y = getListOfErrorFromModel("NNFA", errorName), jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(200,200,0)'),
                name = "NNFA", boxmean = TRUE)
  }
    
  if(choosenModelNames[5] == TRUE)
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
  
  errorMean <- vector()
  for(i in 1 : length(errorModelNames))
  {
    if(choosenModelNames[i] == TRUE) errorMean = c(errorMean, mean(getListOfErrorFromModel(errorModelNames[i], errorName)))
  }
  errorMean
}


getErrorMetricCompare <- function()
{
  comparison()
  
  names = vector()
  for(i in 1 : length(errorModelNames))
  {
     if(choosenModelNames[i] == TRUE) names = c(names, errorModelNames[i])
  }
  
  data.table(NAME = names, MSE = getMeanErrorVectorFromModels("mse"), RMSE =  getMeanErrorVectorFromModels("rmse"), SMAPE = getMeanErrorVectorFromModels("smape"))
}


getCoef <- function(id)
{
  comparison()
  
  n1 = rev(getNeuralNetwork(id)$weights[[1]][[1]][,1])
  n2 = rev(getNeuralNetwork(NULL)$weights[[1]][[1]][,1])

  n1 <- n1[!is.na(n1)] # remove bias
  n2 <- n2[!is.na(n2)]
  arc =  autoRegressiveModels[[id]]$coef #[1 : data.windowSize] # remove error bias
  
  names = NULL
  for(i in 1 : length(arc))
  {
    names = c(names,paste(c("x", i), collapse = ""))
  }
  data.table(Variables = names, AutoRegression = arc, "NN for each" = n1, "NN for all" = n2)
}



getForecastComparisionPlot <- function(id) {
  data.length <- length(data.sets[[id]]$y)
  startRealData <- max(1, data.length - 2 * data.horizon + 1)
  startPredictionIndex = data.length - startRealData - data.horizon + 1
  
  # Start with original data
  prediction <- data.table(x = data.sets[[id]]$x[startRealData:data.length],
    y = data.sets[[id]]$y[startRealData:data.length])
  
  # Add Auto Regression
  prediction$ar <- append(rep(NA, data.horizon), autoRegressiveModels[[id]]$result)
  prediction$ar[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Neural Network for each
  prediction$nnfe <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id)$net.result)
  prediction$nnfe[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Auto Regression for each with hidden layers
  prediction$nnfeh <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.result)
  prediction$nnfeh[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Auto Regression for all
  prediction$nnfa <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id, forAll = TRUE)$net.result)
  prediction$nnfa[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Add Auto Regression for all with hidden layers
  prediction$nnfah <- append(rep(NA, data.horizon),
    getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
  prediction$nnfah[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Plot the data
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)')) %>%
    add_trace(y = ~ar, name = 'Auto Regression', line = list(color = 'rgb(255, 0, 0)')) %>%
    add_trace(y = ~nnfe, name = 'Neural Network /1', line = list(color = 'rgb(255, 150, 0)')) %>%
    add_trace(y = ~nnfeh, name = 'Neural Network /1 hidden', line = list(color = 'rgb(0, 255, 255)')) %>%
    add_trace(y = ~nnfa, name = 'Neural Network /n', line = list(color = 'rgb(0, 0, 255)')) %>%
    add_trace(y = ~nnfah, name = 'Neural Network /n hidden', line = list(color = 'rgb(255, 0, 225)'))
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <- function()
{
  errorTable <<- NULL
}



