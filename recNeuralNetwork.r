 library(RSNNS)
 
 reccurentNeuralNetwork <- NULL
 

 
 
trainRNN <- function(id,  hiddenLayers = c(0))
{
  
  set.seed(1)
  trainset <- getTrainSet(id)
  #learnFunc = "Std_Backpropagation", , size = neuralNetwork.hiddenLayers,  maxit = 500, linOut = FALSE
  rnn <- RSNNS::elman(x = trainset[,2:length(trainset)], y = trainset[,1], size = neuralNetwork.hiddenLayers)
 
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