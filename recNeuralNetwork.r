 library(RSNNS)
 
 reccurentNeuralNetwork <- NULL
 

 
 
trainRNN <- function(trainset,  hiddenLayers = c(0))
{
  rnn <- RSNNS::elman(x = dftrain, y = ytrain, size = hiddenLayers, learnFunc = "Std_Backpropagation", maxit = 500, linOut = FALSE)
  
  
  
  rnn
}

testRNN <- function()
{
  
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