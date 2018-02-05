 library(RSNNS)
 
 validationset.ratio <<- 0.05
 
 
trainRNN <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  #trainset <- getNormalizedTrainSet(id)
  trainset <- getTrainSet(id)
  traininput <- trainset[, 2:length(trainset)]
  traintarget <- trainset[, 1]

  rnn <- RSNNS::elman(x = traininput, y = traintarget, size = 20,# size = vars$options$hiddenLayers,
    learnFuncParams=c(0.1), maxit = 1000,
    linOut = TRUE)
  #rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  
  return(rnn)
}

getModel.elman <- function(id)
{
  trainRNN(id, vars$options$hiddenLayers)
}

getTestResults.elman <- function(model, id)
{
  #datanew <- predict(model, getNormalizedTestSet(id))[,1]
  #denormalized <- denormalizeData(datanew, normalizationParam)
  #denormalized[,1]
  predict(model, getTestSet(id))[,1]
}
