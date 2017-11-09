library(neuralnet)

trainNeuralNetwork <- function(trainset) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  window_size <- length(trainset) - 1
  
  hidden_layer = c(3,2) #set the hidden layer vector
  # Calculate Bias Neuron weights
  excluded_weights = c(1) # first bias needs to be excluded every time
  if(hidden_layer[1] > 0){ #only if hidden layer are present
    
    layer_vector <- NULL
    layer_vector[1] <- window_size
    for(i in 1:length(hidden_layer)){
      layer_vector[i+1] <- hidden_layer[i]
    }
    current_bias <- 1
    exclude_counter <- 2
    for (i in 1:length(hidden_layer)){
      for (j in 1:hidden_layer[i]){
        current_bias <- current_bias + layer_vector[i] + 1
        excluded_weights[exclude_counter] <- current_bias
        exclude_counter <- exclude_counter + 1
      }
    }
  }
  str(excluded_weights)
  
  neuralnet(f, trainset, hidden = hidden_layer, linear.output = TRUE, exclude = excluded_weights)
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
