library(neuralnet)

trainNeuralNetwork <- function(trainset) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  neuralnet(f, trainset, hidden = 0, linear.output = TRUE)
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
