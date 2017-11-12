library(neuralnet)

neuralNetwork.forEach <- NULL
neuralNetwork.forAll <- NULL

trainNeuralNetwork <- function(trainset) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  neuralnet(f, trainset, hidden = 0, linear.output = TRUE)
}

trainNeuralNetworks <- function()
{
  data.trainSets.ids <- names(data.trainSets)
  for (i in 1:length(data.trainSets.ids))
  {
    id <- data.trainSets.ids[i]
    
    neuralNetwork.forEach[[id]] <<- trainNeuralNetwork(data.trainSets[[id]])
  }
  
  neuralNetwork.forAll <<- trainNeuralNetwork(rbindlist(data.trainSets))
}

testNeuralNetwork <- function(neuralNetwork, testset, scale, offset) {
  expected <- testset$xt0
  testset$xt0 <- NULL
  
  n <- compute(neuralNetwork, testset)
  
  n$net.result <- n$net.result * scale + offset
  n$net.expected <- expected * scale + offset
  n$net.mse <- sum((n$net.expected - n$net.result)^2)/nrow(n$net.result)
  
  n
}
