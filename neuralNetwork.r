library(neuralnet)

neuralNetwork.forEach <- NULL
neuralNetwork.forEach.hiddenLayers <- NULL
neuralNetwork.forAll <- NULL
neuralNetwork.forAll.hiddenLayers <- NULL

neuralNetwork.testResults.forEach <- NULL
neuralNetwork.testResults.forEach.hiddenLayers <- NULL
neuralNetwork.testResults.forAll <- NULL
neuralNetwork.testResults.forAll.hiddenLayers <- NULL

trainNeuralNetwork <- function(trainset, excludeBias, hiddenLayers = c(0)) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  if(excludeBias) {
    window_size <- length(trainset) - 1
    
    # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
    excluded_weights = c(1) # first bias needs to be excluded every time
    if(hiddenLayers[1] > 0){ #only if hidden layer are present
      
      layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hiddenLayers)...))
      layer_vector[1] <- window_size #number of input neurons
      for(i in 1:length(hiddenLayers)){
        layer_vector[i+1] <- hiddenLayers[i] #create the layer vector
      }
      current_bias <- 1 #first bias always 1
      exclude_counter <- 2 #to fill the exclude-vector. first one is already filled (1), so begin with 2
      for (i in 1:length(hiddenLayers)){ #iterate through the vertical layers, beginning with input nodes
        for (j in 1:hiddenLayers[i]){ #iterate through the nodes of the current layer
          current_bias <- current_bias + layer_vector[i] + 1 #from bias to bias calculate new_value = old_value + n + 1 where n is number of nodes in current layer
          excluded_weights[exclude_counter] <- current_bias #add to vector for excluded weights
          exclude_counter <- exclude_counter + 1 #go to next element in exclude vector
        }
      }
    }
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, exclude = excluded_weights)
  }
  else {
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE)
  }
}

trainNeuralNetworks <- function(excludeBias)
{
  data.trainSets.ids <- names(data.trainSets)
  
  numNN <- 2 * length(data.trainSets.ids) + 2
  
  for (i in 1:length(data.trainSets.ids))
  {
    id <- data.trainSets.ids[i]
    
    neuralNetwork.forEach[[id]] <<- trainNeuralNetwork(data.trainSets[[id]], excludeBias)
    incProgress(1/numNN, detail = paste('ID', id, '(without hidden layers)'))
    neuralNetwork.forEach.hiddenLayers[[id]] <<- trainNeuralNetwork(data.trainSets[[id]], excludeBias, c(0))
    incProgress(1/numNN, detail = paste('ID', id, '(with hidden layers)'))
  }
  
  trainSetsCombined <- rbindlist(data.trainSets)
  neuralNetwork.forAll <<- trainNeuralNetwork(trainSetsCombined, excludeBias)
  incProgress(2/numNN, detail = 'General for all (without hidden layers)')
  neuralNetwork.forAll.hiddenLayers <<- trainNeuralNetwork(trainSetsCombined, excludeBias, c(0))
  incProgress(2/numNN, detail = 'General for all (with hidden layers)')
}

testNeuralNetwork <- function(neuralNetwork, testSetID) {
  expected <- data.testSets[[testSetID]]$xt0
  testData <- data.testSets[[testSetID]]
  testData$xt0 <- NULL
  
  scale <- data.normalizationInfo[[testSetID]]$scale
  offset <- data.normalizationInfo[[testSetID]]$offset
  
  n <- compute(neuralNetwork, testData)
  
  n$net.result <- n$net.result * scale + offset
  n$net.expected <- expected * scale + offset
  n$net.mse <- sum((n$net.expected - n$net.result)^2)/nrow(n$net.result)
  
  n
}

testNeuralNetworks <- function() {
  neuralNetwork.forEach.ids <- names(neuralNetwork.forEach)
  for (i in 1:length(neuralNetwork.forEach.ids))
  {
    id <- neuralNetwork.forEach.ids[i]
    
    neuralNetwork.testResults.forEach[[id]] <<- testNeuralNetwork(neuralNetwork.forEach[[id]], id)
    neuralNetwork.testResults.forEach.hiddenLayers[[id]] <<- testNeuralNetwork(neuralNetwork.forEach.hiddenLayers[[id]], id)
    neuralNetwork.testResults.forAll[[id]] <<- testNeuralNetwork(neuralNetwork.forAll, id)
    neuralNetwork.testResults.forAll.hiddenLayers[[id]] <<- testNeuralNetwork(neuralNetwork.forAll.hiddenLayers, id)
  }
}
