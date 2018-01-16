 library(RSNNS)
 
 validationset.ratio <<- 0.05
 
 
trainRNN <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=validationset.ratio)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)

  rnn <- RSNNS::elman(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers,
                    inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
                    maxit = 500, learnFunc = "JE_Rprop")
  rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = vars$options$hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(rnn)
}

getModel.elman <- function(id)
{
  trainRNN(id, vars$options$hiddenLayers)
}

testRNN <- function(model, id)
{
  predict(model, getTestSet(id))[,1]
}

getTestResults.elman <- function(id)
{
  testRNN(getModel('elman', id), id)
}

# Multilayer Perceptron (Feed Forward Network) with the RSNNS Package to compare with neuralnet

trainMLP <- function(id, hiddenLayers = TRUE)
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

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

getModel.mlp <- function(id)
{
  trainMLP(id, FALSE)
}

getModel.mlph <- function(id)
{
  trainMLP(id, TRUE)
}

testMLP <- function(model, id)
{
  predict(model, getTestSet(id))[,1]
}

getTestResults.mlp <- function(id)
{
  testMLP(getModel('mlp', id), id)
}

getTestResults.mlph <- function(id)
{
  testMLP(getModel('mlph', id), id)
}

####### Train and Test Jordan Network

trainJordan <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=validationset.ratio)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)

  jordan <- RSNNS::jordan(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers,
                    inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
                    maxit = 500, learnFunc = "JE_Rprop")
  #rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = vars$options$hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(jordan)
}

getModel.jordan <- function(id)
{
  trainJordan(id, vars$options$hiddenLayers)
}

testJordan <- function(model, id)
{
  predict(model, getTestSet(id))[,1]
}

getTestResults.jordan <- function(id)
{
  testJordan(getModel('jordan', id), id)
}
