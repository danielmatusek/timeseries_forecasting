library(neuralnet)

trainNeuralNetwork <- function(trainset, input) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  hidden_layer = c(0) #set the hidden layer vector
  
  if(input$biasCheckbox == TRUE){
    window_size <- length(trainset) - 1
    
    # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
    excluded_weights = c(1) # first bias needs to be excluded every time
    if(hidden_layer[1] > 0){ #only if hidden layer are present
      
      layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hidden_layer)...))
      layer_vector[1] <- window_size #number of input neurons
      for(i in 1:length(hidden_layer)){
        layer_vector[i+1] <- hidden_layer[i] #create the layer vector
      }
      current_bias <- 1 #first bias always 1
      exclude_counter <- 2 #to fill the exclude-vector. first one is already filled (1), so begin with 2
      for (i in 1:length(hidden_layer)){ #iterate through the vertical layers, beginning with input nodes
        for (j in 1:hidden_layer[i]){ #iterate through the nodes of the current layer
          current_bias <- current_bias + layer_vector[i] + 1 #from bias to bias calculate new_value = old_value + n + 1 where n is number of nodes in current layer
          excluded_weights[exclude_counter] <- current_bias #add to vector for excluded weights
          exclude_counter <- exclude_counter + 1 #go to next element in exclude vector
        }
      }
    }
    neuralnet(f, trainset, hidden = hidden_layer, linear.output = TRUE, exclude = excluded_weights)
  }
  else {
    neuralnet(f, trainset, hidden = hidden_layer, linear.output = TRUE)
  }
  
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
