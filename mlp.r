library(RSNNS)


getModel.mlp <- function(id, hiddenLayers = FALSE)
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[, 2:length(trainset)]
  traintarget <- trainset[, 1]
  
  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=0.1)
  myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)
  
  if(hiddenLayers)
  {
    mlp <- RSNNS::mlp(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers, learnFuncParams=c(0.05),
      inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFunc = "Rprop",
      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  } else {
    mlp <- RSNNS::mlp(x = myset$inputsTrain, y = myset$targetsTrain, size = NULL, learnFuncParams=c(0.05),
      inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFunc = "Rprop",
      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  }
  #learnFunc = "Std_Backpropagation", , size = vars$options$hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(mlp)
}

getModel.mlph <- function(id)
{
  getModel.mlp(id, TRUE)
}

getTestResults.mlp <- function(model, id)
{
  predict(model, getTestSet(id))[,1]
}
getTestResults.mlph <- getTestResults.mlp
