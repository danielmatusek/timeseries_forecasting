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

getTestResults.mlp <- function(model, id)
{
  #datanew <- predict(model, getNormalizedTestSet(id))[,1]
  #denormalized <- denormalizeData(datanew, normalizationParam)
  #denormalized[,1]
  predict(model, getTestSet(id))[,1]
}
getTestResults.mlph <- getTestResults.mlp
