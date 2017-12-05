library(plotly)
library(forecast)
library(stats)
library(data.table)

aRModelName <- NULL

learnARModel <- function(id)
{

  #print(paste("AR", id, sep=" "))
  start.time <- Sys.time()
  print(paste('train ar for id', id))
  
  y <- data.sets[[id]]$y
  
  spl <<- length(y) - data.horizon
  trainData <- y[(1 : spl)]
  testData <- y[-(1 : spl)]

  if(aRModelName == "AR")
  {
    arModel <- stats::ar(ts(trainData), aic = FALSE, data.windowSize, method = "burg", demean = !neuralNetwork.excludeBias)
    end.time <- Sys.time()
    coef <- arModel$ar
  }
  else if (aRModelName == "AutoArima")
  {
    arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
    end.time <- Sys.time()
    coef <- arModel$coef
  } 
  else if (aRModelName == "ManualAutoArima" || aRModelName == "ManualAR")
  {
    #get coefficients
    if(aRModelName == "ManualAutoArima"){
      arModel <- auto.arima(ts(trainData), start.p = data.windowSize, max.p = data.windowSize, d = 0, max.q = 0)
      coef <- arModel$coef
    } else {
      arModel <- stats::ar(ts(trainData), aic = FALSE, data.windowSize, method = "burg", demean = !neuralNetwork.excludeBias)
      coef <- arModel$ar
    }
    #create output datatable
    ar_manual_result <- data.frame(V0 = double())
    end.time <- Sys.time()
    #offset for forecast to use whole input data.table
    ar_offset <- length(trainData) + 1
    
    if(aRModelName== "ManualAutoArima"){
      timesSum <- (length(coef)-1)
    } else timesSum <- (length(coef))
    end.time <- Sys.time()
    for(i in 1:length(testData)){
      y_t_before <- 0
      for(j in 1:timesSum){
        y_t <- y_t_before + (coef[j] * y[ar_offset - j])
        y_t_before <- y_t
      }
      #y_t <- y_t_before + coef[length(coef)] ## -----> Error value for Auto.Arima)
      temp_df <- data.frame(y_t)
      names(temp_df) <- c("V0")
      #append new forecasted value to table
      ar_manual_result <- rbind(ar_manual_result, temp_df)
      #move window one to the right, old y_t will be y_t-1
      ar_offset <- ar_offset + 1
    }
  }

  if(aRModelName == "AutoArima" | aRModelName == "AR"){
    tsPred <- predict(arModel, n.ahead = data.horizon)
    end.time <- Sys.time()
    model <- list(model = arModel, coef = coef, trained = trainData, result = tsPred$pred, expected = testData)
  }
  else {
    model <- list(model = arModel, coef = coef, trained = trainData, result = ar_manual_result$V0, expected = testData)
    end.time <- Sys.time(Sys.time())
  }
  
  autoRegressiveModels[[id]] <<- model
  function.time <<- end.time - start.time
}

getARModel <- function(id) {
  if (is.null(autoRegressiveModels[[id]]))
  {
    learnARModel(id)
    
  }
  
  return (autoRegressiveModels[[id]])
}

getARCoef <- function(id)
{
  arModel <- getARModel(id)$model
  
  if(aRModelName == 'AR' || aRModelName == 'ManualAutoArima')
  {
    c(arModel$ar, arModel$x.mean)
  }
  else
  {
    arModel$coef
  }
}

resetARModels <- function()
{
  autoRegressiveModels <<- NULL
}
