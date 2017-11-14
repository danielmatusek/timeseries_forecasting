library(ggplot2)
library(plotly)
source('autoRegression.r')
source('neuralNetwork.r')
source('global.r')





ar.MSES <<- vector()
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

boxplotComarision <- function(window, predValue)
{
  ids = names(data.sets)
  len = length(ids)

  for(i in 1 : len)
  {
    id = ids[i]
    cdata = data.sets[[id]]$y
    
    getARModel(cdata, window, predValue)
    error  = error_metric(model$expected, model$result)
    ar.MSES <<- c(ar.MSES, error$mse)
    ar.RMSES <<- c(ar.RMSES, error$rmse)
    ar.SMAPES <<- c(ar.SMAPES, error$smape)
    
    error = error_metric(neuralNetwork.testResults.forEach[[i]]$net.expected, neuralNetwork.testResults.forEach[[i]]$net.result)
    nn.MSES <<- c(nn.MSES, error$mse)
    nn.RMSES <<- c(nn.RMSES, error$rmse)
    nn.SMAPES <<- c(nn.SMAPES, error$smape)
    
    error = error_metric(neuralNetwork.testResults.forEach.hiddenLayers[[i]]$net.expected, neuralNetwork.testResults.forEach.hiddenLayers[[i]]$net.result)
    nnh.MSES <<- c(nnh.MSES, error$mse)
    nnh.RMSES <<- c(nnh.RMSES, error$rmse)
    nnh.SMAPES <<- c(nnh.SMAPES, error$smape)
    
    error = error_metric(neuralNetwork.testResults.forAll[[i]]$net.expected, neuralNetwork.testResults.forAll[[i]]$net.result)
    nnfa.MSES <<- c(nnfa.MSES, error$mse)
    nnfa.RMSES <<- c(nnfa.RMSES, error$rmse)
    nnfa.SMAPES <<- c(nnfa.SMAPES, error$smape)
    
    error = error_metric(neuralNetwork.testResults.forAll.hiddenLayers[[i]]$net.expected, neuralNetwork.testResults.forAll.hiddenLayers[[i]]$net.result)
    nnhfa.MSES <<- c(nnhfa.MSES, error$mse)
    nnhfa.RMSES <<- c(nnhfa.RMSES, error$rmse)
    nnhfa.SMAPES <<- c(nnhfa.SMAPES, error$smape)
  }
}

getBoxplot <- function(errorName)
{
  
  errorModel <<-  NULL
  if(errorName == 'MSE')
  {
    errorModel.ar <<- ar.MSES
    errorModel.nn <<- nn.MSES
    errorModel.nnh <<- nnh.MSES
    errorModel.nnfa <<- nnfa.MSES
    errorModel.nnhfa <<- nnhfa.MSES
  }
  else if( errorName == 'RMSE')
  {
    errorModel.ar <<- ar.RMSES
    errorModel.nn <<- nn.RMSES
    errorModel.nnh <<- nnh.RMSES
    errorModel.nnfa <<- nnfa.RMSES
    errorModel.nnhfa <<- nnhfa.RMSES
  }
  else if(errorName == 'SMAPE')
  {
    errorModel.ar <<- ar.SMAPES
    errorModel.nn <<- nn.SMAPES
    errorModel.nnh <<- nnh.SMAPES
    errorModel.nnfa <<- nnfa.SMAPES
    errorModel.nnhfa <<- nnhfa.SMAPES
  }
  
  p <- plot_ly(type='box')%>%
    add_boxplot(x = errorModel.ar, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(5,40,89)'),
                name = " AR")%>%
    add_boxplot(x = errorModel.nn, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(50,40,89)'),
                name = " NN")%>%
    add_boxplot(x = errorModel.nnh, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(100,40,89)'),
                name = " NNH")%>%
    add_boxplot(x = errorModel.nnfa, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(150,40,89)'),
                name = " NNFA")%>%
    add_boxplot(x = errorModel.nnhfa, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(200,40,89)'),
                name = " NNHFA")
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

