 library(RSNNS)
 
 reccurentNeuralNetwork <- NULL
 

 
 
trainRNN <- function(id,  hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]

  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  rnn <- RSNNS::elman(x = traininput, y = traintarget, size = neuralNetwork.hiddenLayers)
 
  return(rnn)
}

testRNN <- function(model, id)
{
  testset <- getTestSet(id)
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

trainMLP <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  mlp <- RSNNS::mlp(x = trainset[,2:length(trainset)], y = trainset[,1], size = neuralNetwork.hiddenLayers, initFunc = "Randomize_Weights", 
        learnFunc = "Std_Backpropagation", linOut = TRUE)
 
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