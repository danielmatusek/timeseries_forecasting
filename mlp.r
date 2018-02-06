library(RSNNS)


getModel.mlp <- function(id, hiddenLayers = FALSE)
{
  set.seed(1)
  #trainset <- getNormalizedTrainSet(id)
  trainset <- getTrainSet(id)
  traininput <- trainset[, 2:length(trainset)]
  traintarget <- trainset[, 1]
  
  if(hiddenLayers)
  {
    mlp <- RSNNS::mlp(x = traininput, y = traintarget, size = vars$options$hiddenLayers, learnFuncParams=c(0.05),
      learnFunc = "Rprop",
      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  } else {
    mlp <- RSNNS::mlp(x = traininput, y = traintarget, size = NULL, learnFuncParams=c(0.05),
      learnFunc = "Rprop",
      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  }
  
  return(mlp)
}

getModel.mlph <- function(id)
{
  getModel.mlp(id, TRUE)
}





getModel.mlpei <- function(id)
{
  baseModelName <- 'mlp'
  externalError = list()
  excludedPath <- NULL
  bestModel = getModel(baseModelName, id)
  bestModel$excludedInput <- NULL
  
  if (mode(bestModel) == 'logical')
  {
    externalError[['empty']] <- NA
    bestExclInput <- NULL
    bestError <- .Machine$double.xmax
  }
  else
  {
    testResults <- getTestResults(baseModelName, id)
    smape <- sMAPE(testResults$expected, testResults$predicted)
    externalError[['empty']] <- if(!is.nan(smape)){ smape }else{ 0 }
    bestExclInput <- 'empty'
    bestError <- externalError[['empty']]
  }
  queue <- lapply(1 : vars$options$windowSize, function(i) { i })
  
  trainSet <- getTrainSet(id)
  testSet <- data.expectedTestResults[[id]]
  
  repeat
  {
    newBestInput <- NULL
    while(length(queue) > 0)
    {
      path <- queue[[1]]
      
      if(length(path) == vars$options$windowSize) break
      
      vec = c(1, (path + 1))
      traininput <- data.matrix(data.matrix(trainSet)[,-vec])
      traintarget <- trainSet[, 1]
      
      mlp <- RSNNS::mlp(x = traininput, y = traintarget, size = NULL, learnFuncParams=c(0.05),
                        learnFunc = "Rprop",
                        linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
      
      mlp$excludedInput <- path
      
      pathAsString <- paste(path, collapse = ",")
      
      if (mode(mlp) == 'logical')
      {
        externalError[[pathAsString]] <- NA
      }
      else
      {
        
        smape <- sMAPE(testSet, getTestResults.mlpei(mlp, id))
        externalError[[pathAsString]] <-  if(!is.nan(smape)){ smape }else{ 0 }
        
        error <- externalError[[pathAsString]]
        if (error < bestError)
        {
          bestModel <- mlp
          newBestInput <- path
          excludedPath <- path
          bestError <- externalError[[pathAsString]]
        }
      }
      
      if(length(queue) == 1)
      {
        queue <- NULL
        break
      }
      
      queue <- queue[-1]
    }
    
    if (is.null(newBestInput))
    {
      break
    }
    else
    {
      newInputs <- setdiff(1 : vars$options$windowSize, newBestInput)
      queue <- lapply(newInputs, function(newInput) { c(newBestInput, newInput) })
    }
  }
  
  
  if (mode(bestModel) != 'logical')
  {
    bestModel$outSampleError <- externalError
    bestModel$path <- excludedPath
  }
  else
  {
    bestModel <- NA
  }
  
  return(bestModel)
}



getTestResults.mlpei <- function(model, id)
{
  data <- getTestSet(id)
  if(!is.null(model$excludedInput)) data <- data.matrix(data.matrix(data)[, -model$excludedInput])
  datanew <- predict(model, data)[,1]
  #denormalized <- denormalizeData(datanew, normalizationParam)
  #denormalized[,1]
}


getTestResults.mlp <- function(model, id)
{
  #datanew <- predict(model, getNormalizedTestSet(id))[,1]
  #denormalized <- denormalizeData(datanew, normalizationParam)
  #denormalized[,1]
  predict(model, getTestSet(id))[,1]
}

getTestResults.mlph <- getTestResults.mlp
