 library(RSNNS)
 
 validationset.ratio <<- 0.05
 
# trains a Elman network of the RSNNS package
# --id: ID of the time series
# --hiddenLayers: vector of the hidden layers 
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

# get the model for the Elman network of the RSNNS package
# --id: id of the time series
getModel.elman <- function(id)
{
  trainRNN(id, vars$options$hiddenLayers)
}

# get the test results for a Elman network
# --model: the model object
# --id: id of the time series
getTestResults.elman <- function(model, id)
{
  testSet <- getTestSet(id, normalization = '0_1')
  datanew <- predict(model, testSet)[,1]
  denormalized <- denormalizeData(datanew, getNormParameters(model))
  denormalized[,1]
}