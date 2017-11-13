
library(ggplot2)
library(plotly)
source('autoRegression.r')
source('neuralNetwork.r')


boxplotComarision <- function(db,window, predValue)
{
  meterids <- names(db)
  
  len = length(meterids)
  MSES <<- vector()
  RMSES <<- vector()
  SMAPES <<- vector()
  
  
  for(i in 1 : length(meterids))
  {
    meterid = meterids[i]
    cdata = db[[meterid]]$consumption
    ARModel(cdata, window, predValue)
    error  = error_metric_AR()
    MSES <<- c(MSES, error$mse)
    RMSES <<- c(RMSES, error$rmse)
    SMAPES <<- c(SMAPES, error$smape)
  }
  
  
  
  p <- plot_ly(type='box')%>%
  add_boxplot(x = MSES, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
              marker = list(color = 'rgb(7,40,89)'),
              line = list(color = 'rgb(7,40,89)'),
              name = "MSE AR")
  p$elementId <- NULL
  p
  
}

error_metric_compare <- function()
{
  data.frame(MSE = mean(MSES), RMSE =  mean(RMSES), SMAPE = mean(SMAPES))
}

