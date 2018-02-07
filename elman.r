 library(RSNNS)
 
 validationset.ratio <<- 0.05
 
 
trainRNN <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id, normalization = '0_1')
  #trainset <- getTrainSet(id)
  traininput <- trainset[, -1]
  traintarget <- trainset[, 1]

  rnn <- RSNNS::elman(x = traininput, y = traintarget, size = vars$options$hiddenLayers,
    learnFuncParams=c(0.1), maxit = 1000, linOut = TRUE)
  #rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  
  attr(rnn, 'normParams') <- getNormParameters(trainset)
  return(rnn)
}

getModel.elman <- function(id)
{
  trainRNN(id, vars$options$hiddenLayers)
}

getTestResults.elman <- function(model, id)
{
  testSet <- getTestSet(id, normalization = '0_1')
  datanew <- predict(model, testSet)[,1]
  denormalized <- denormalizeData(datanew, getNormParameters(model))
  denormalized[,1]
}