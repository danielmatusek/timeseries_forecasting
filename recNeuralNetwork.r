 library(RSNNS)
 
 reccurentNeuralNetwork <- NULL

 rsnns.rnn <<- TRUE
 rsnns.mlp <<- TRUE
 
 
trainRNN <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=0.1)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)

  rnn <- RSNNS::elman(x = myset$inputsTrain, y = myset$targetsTrain, size = neuralNetwork.hiddenLayers,
                    inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
                    maxit = 500)
  rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(rnn)
}

testRNN <- function(model, id)
{
  testset <- getTestSet(id)
  print(testset)
  expected <- testset[,1]
  
  result <- predict(model, testset[,2 : length(testset)])
  
  
  mse <- sum((expected - result)^2) / nrow(result)
  structure(list(expected = expected, result = result, mse = mse), class = 'TestResults')
}


getRNN <- function(id)
{
  
}

getRNNPlot <- function()
{
  
}

resetRNN <- function()
{
  
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
    mlp <- RSNNS::mlp(x = myset$inputsTrain, y = myset$targetsTrain, size = neuralNetwork.hiddenLayers, learnFuncParams=c(0.05),
                      inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFunc = "Rprop",
                      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  } else {
    mlp <- RSNNS::mlp(x = myset$inputsTrain, y = myset$targetsTrain, size = NULL, learnFuncParams=c(0.05),
                      inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFunc = "Rprop",
                      linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  }
  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(mlp)
}

testMLP <- function(model, id)
{
  testset <- getTestSet(id)
  expected <- testset[,1]
  
  result <- predict(model, testset[,2 : length(testset)])
  
  
  mse <- sum((expected - result)^2) / nrow(result)
  structure(list(expected = expected, result = result, mse = mse), class = 'TestResults')
}

getMLP <- function(x)
{

}

getMLPPlot <- function(x)
{

}

resetMLP <- function(x)
{

}

####### Train and Test Jordan Network

trainJordan <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=0.1)
  myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = TRUE)

  jordan <- RSNNS::jordan(x = myset$inputsTrain, y = myset$targetsTrain, size = neuralNetwork.hiddenLayers,
                    inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
                    maxit = 500)
  #rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(jordan)
}

testJordan <- function(model, id)
{
  testset <- getTestSet(id)
  expected <- testset[,1]
  
  result <- predict(model, testset[,2 : length(testset)])  
  
  mse <- sum((expected - result)^2) / nrow(result)
  structure(list(expected = expected, result = result, mse = mse), class = 'TestResults')
}

getJordan <- function(x)
{

}

getJordanPlot <- function(x)
{

}

resetJordan <- function(x)
{

}