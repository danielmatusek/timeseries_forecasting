library(ggplot2)
library(plotly)
source('autoRegression.r')
source('neuralNetwork.r')
source('global.r')


ar.MSES <- NULL

comparison <- function()
{
  
  if(is.null(ar.MSES))
  {
    resetComparison()
    
    ids = names(data.sets)
    len = length(ids)
    
    for(i in 1 : len)
    {
      id = ids[i]

      error  = error_metric(autoRegressiveModels[[id]]$expected, autoRegressiveModels[[id]]$result)
      ar.MSES <<- c(ar.MSES, error$mse)
      ar.RMSES <<- c(ar.RMSES, error$rmse)
      ar.SMAPES <<- c(ar.SMAPES, error$smape)
      
      #browser()
      typ <- neuralNetwork.type
            
 #     for(j in 1 : length(typ)){
#        if(typ[[j]] == "forecast_one"){
          error = error_metric(getNeuralNetworkTestResults(id)$net.expected, getNeuralNetworkTestResults(id)$net.result)
          nn.MSES <<- c(nn.MSES, error$mse)
          nn.RMSES <<- c(nn.RMSES, error$rmse)
          nn.SMAPES <<- c(nn.SMAPES, error$smape)
#        }
        
#        else if(typ[[j]] == "forecast_one_hidden"){
          error = error_metric(getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, hiddenLayers = TRUE)$net.result)
          nnh.MSES <<- c(nnh.MSES, error$mse)
          nnh.RMSES <<- c(nnh.RMSES, error$rmse)
          nnh.SMAPES <<- c(nnh.SMAPES, error$smape)
#        }
#        else if(typ[[j]] == "forecast_all"){
          error = error_metric(getNeuralNetworkTestResults(id, forAll = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE)$net.result)
          nnfa.MSES <<- c(nnfa.MSES, error$mse)
          nnfa.RMSES <<- c(nnfa.RMSES, error$rmse)
          nnfa.SMAPES <<- c(nnfa.SMAPES, error$smape)
#        }
        #else if(typ[[j]] == "forecast_all_hidden"){
        #  error = error_metric(getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.expected, getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
        #  nnhfa.MSES <<- c(nnhfa.MSES, error$mse)
        #  nnhfa.RMSES <<- c(nnhfa.RMSES, error$rmse)
        #  nnhfa.SMAPES <<- c(nnhfa.SMAPES, error$smape)
        #}
#      }
    }
  }
  
}

getBoxplot <- function(errorName)
{
  
  errorModel <<-  NULL
  typ <- neuralNetwork.type
  
#  for(j in 1 : length(typ)){
  if(errorName == 'MSE')
  {
    errorModel.ar <<- ar.MSES
    #browser()
 #   if(typ[[j]] == "forecast_one") 
    errorModel.nn <<- nn.MSES
#    else if(typ[[j]] == "forecast_one_hidden") 
    errorModel.nnh <<- nnh.MSES
 #   else if(typ[[j]] == "forecast_all") 
    errorModel.nnfa <<- nnfa.MSES
    #errorModel.nnhfa <<- nnhfa.MSES
  }
  else if( errorName == 'RMSE')
  {
    errorModel.ar <<- ar.RMSES
    errorModel.nn <<- nn.RMSES
    errorModel.nnh <<- nnh.RMSES
    errorModel.nnfa <<- nnfa.RMSES
    #errorModel.nnhfa <<- nnhfa.RMSES
  }
  else if(errorName == 'SMAPE')
  {
    errorModel.ar <<- ar.SMAPES
    errorModel.nn <<- nn.SMAPES
    errorModel.nnh <<- nnh.SMAPES
    errorModel.nnfa <<- nnfa.SMAPES
    #errorModel.nnhfa <<- nnhfa.SMAPES
  }
#  }
#  for (i in 1:length(typ)){
#  if(typ[[i]] == "forecast_one"){
#    pnn <- add_boxplot(y = errorModel.nn, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
#                marker = list(color = 'rgb(7,40,89)'),
#                line = list(color = 'rgb(0,200,0)'),
#                name = " NN", boxmean=TRUE)
#    }
#  else if(typ[[i]] == "forecast_one_hidden"){
#    pnnh <- add_boxplot(y = errorModel.nnh, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
#                marker = list(color = 'rgb(7,40,89)'),
#                line = list(color = 'rgb(0,0,200)'),
#                name = " NNH", boxmean=TRUE)
#  }
#  else if(typ[[i]] == "forecast_all"){
#    pnnfa <- add_boxplot(y = errorModel.nnfa, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
#                marker = list(color = 'rgb(7,40,89)'),
#                line = list(color = 'rgb(200,200,0)'),
#                name = " NNFA", boxmean=TRUE)
#  }
#}
  p <- plot_ly(type='box')%>%
    add_boxplot(y = errorModel.ar, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(200,0,0)'),
                name = " AR", boxmean=TRUE)%>%
    add_boxplot(y = errorModel.nn, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,200,0)'),
                name = " NN", boxmean=TRUE)%>%
    add_boxplot(y = errorModel.nnh, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(0,0,200)'),
                name = " NNH", boxmean=TRUE)
    #pnn%>%
    #pnnh%>%
    #ppfa
    
  #%>%
    #add_boxplot(y = errorModel.nnhfa, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
    #            marker = list(color = 'rgb(7,40,89)'),
    #            line = list(color = 'rgb(0,200,200)'),
    #            name = " NNHFA")
  p$elementId <- NULL
  p
}

error_metric_compare <- function()
{
  MSES <-vector()
  RMSES <-vector()
  SMAPES <-vector()

  MSES <- c(MSES, mean(ar.MSES))
  MSES <- c(MSES, mean(nn.MSES))
  MSES <- c(MSES, mean(nnh.MSES))
  MSES <- c(MSES, mean(nnfa.MSES))
  MSES <- c(MSES, mean(nnhfa.MSES))
  
  RMSES <- c(RMSES, mean(ar.RMSES))
  RMSES <- c(RMSES, mean(nn.RMSES))
  RMSES <- c(RMSES, mean(nnh.RMSES))
  RMSES <- c(RMSES, mean(nnfa.RMSES))
  RMSES <- c(RMSES, mean(nnhfa.RMSES))
  
  SMAPES <- c(SMAPES, mean(ar.SMAPES))
  SMAPES <- c(SMAPES, mean(nn.SMAPES))
  SMAPES <- c(SMAPES, mean(nnh.SMAPES))
  SMAPES <- c(SMAPES, mean(nnfa.SMAPES))
  SMAPES <- c(SMAPES, mean(nnhfa.SMAPES))
  
  data.table(NAME = c("AR", "NN", "NNH", "NNFA", "NNHFA"), MSE = MSES, RMSE =  RMSES, SMAPE = SMAPES)
}

getCoef <- function(id)
{
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
  data.table(Variables = names, AutoRegression = arc, "NN for each" = n1 , "NN for all" = n2)
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
  #prediction$nnfah <- append(rep(NA, data.horizon),
  #  getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)$net.result)
  #prediction$nnfah[[startPredictionIndex]] <- prediction$y[[startPredictionIndex]]
  
  # Plot the data
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original', line = list(color = 'rgb(0, 0, 0)')) %>%
    add_trace(y = ~ar, name = 'Auto Regression', line = list(color = 'rgb(255, 0, 0)')) %>%
    add_trace(y = ~nnfe, name = 'Neural Network /1', line = list(color = 'rgb(255, 150, 0)')) %>%
    add_trace(y = ~nnfeh, name = 'Neural Network /1 hidden', line = list(color = 'rgb(0, 255, 255)')) %>%
    add_trace(y = ~nnfa, name = 'Neural Network /n', line = list(color = 'rgb(0, 0, 255)')) #%>%
    #add_trace(y = ~nnfah, name = 'Neural Network /n hidden', line = list(color = 'rgb(255, 0, 225)'))
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

resetComparison <-function()
{
  ar.MSES <<- NULL
  ar.RMSES <<- vector()
  ar.SMAPES <<- vector()
  
  nn.MSES <<- vector()
  nn.RMSES <<- vector()
  nn.SMAPES <<- vector()
  
  nnh.MSES <<- vector()
  nnh.RMSES <<- vector()
  nnh.SMAPES <<- vector()
  
  nnfa.MSES <<- vector()
  nnfa.RMSES <<- vector()
  nnfa.SMAPES <<- vector()
  
  nnhfa.MSES <<- vector()
  nnhfa.RMSES <<- vector()
  nnhfa.SMAPES <<- vector()
}
