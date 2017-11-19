library(neuralnet)

neuralNetwork.excludeBias <<- TRUE
neuralNetwork.hiddenLayers <<- c(0)
neuralNetwork.excludeVector <<- NULL

resetNeuralNetworks.hidden <- function()
{
  neuralNetwork.forEach.hiddenLayers <<- NULL
  neuralNetwork.forAll.hiddenLayers <<- NULL
  
  neuralNetwork.testResults.forEach.hiddenLayers <<- NULL
  neuralNetwork.testResults.forAll.hiddenLayers <<- NULL
}

resetNeuralNetworks <- function()
{
  resetNeuralNetworks.hidden()
  
  neuralNetwork.forEach <<- NULL
  neuralNetwork.forAll <<- NULL
  
  neuralNetwork.testResults.forEach <<- NULL
  neuralNetwork.testResults.forAll <<- NULL
}

setExcludeVector <- function(windowSize) {
  # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
  neuralNetwork.excludeVector <<- c(1) # first bias needs to be excluded every time
  if(neuralNetwork.hiddenLayers[1] > 0){ #only if hidden layer are present
    
    layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hiddenLayers)...))
    layer_vector[1] <- windowSize #number of input neurons
    for(i in 1:length(neuralNetwork.hiddenLayers)){
      layer_vector[i+1] <- neuralNetwork.hiddenLayers[i] #create the layer vector
    }
    current_bias <- 1 #first bias always 1
    exclude_counter <- 2 #to fill the exclude-vector. first one is already filled (1), so begin with 2
    for (i in 1:length(neuralNetwork.hiddenLayers)){ #iterate through the vertical layers, beginning with input nodes
      for (j in 1:neuralNetwork.hiddenLayers[i]){ #iterate through the nodes of the current layer
        current_bias <- current_bias + layer_vector[i] + 1 #from bias to bias calculate new_value = old_value + n + 1 where n is number of nodes in current layer
        neuralNetwork.excludeVector[exclude_counter] <<- current_bias #add to vector for excluded weights
        exclude_counter <- exclude_counter + 1 #go to next element in exclude vector
      }
    }
  }
}

trainNeuralNetwork <- function(trainset, hiddenLayers = c(0)) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  if(neuralNetwork.excludeBias) {
    setExcludeVector(length(trainset) - 1)
    
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE,
      exclude = (if(hiddenLayers == c(0)) c(1) else neuralNetwork.excludeVector))
  }
  else {
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE)
  }
}

# Get the neural network for the given parameters (one neural network for all if is.null(id))
# Compute the neural network if necessary
getNeuralNetwork <- function(id, hiddenLayers = FALSE) {
  if (is.null(id))
  {
    trainSetsCombined <- rbindlist(data.trainSets)
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.forAll.hiddenLayers))
      {
        neuralNetwork.forAll.hiddenLayers <<- trainNeuralNetwork(trainSetsCombined, neuralNetwork.hiddenLayers)
      }
      
      return(neuralNetwork.forAll.hiddenLayers)
    }
    else
    {
      if (is.null(neuralNetwork.forAll))
      {
        neuralNetwork.forAll <<- trainNeuralNetwork(trainSetsCombined)
      }
      
      return(neuralNetwork.forAll)
    }
  }
  else
  {
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.forEach.hiddenLayers[[id]]))
      {
        neuralNetwork.forEach.hiddenLayers[[id]] <<- trainNeuralNetwork(data.trainSets[[id]], neuralNetwork.hiddenLayers)
      }
      
      return(neuralNetwork.forEach.hiddenLayers[[id]])
    }
    else
    {
      if (is.null(neuralNetwork.forEach[[id]]))
      {
        neuralNetwork.forEach[[id]] <<- trainNeuralNetwork(data.trainSets[[id]])
      }
      
      return(neuralNetwork.forEach[[id]])
    }
  }
}

trainNeuralNetworks <- function(excludeBias, hiddenLayers)
{
  neuralNetwork.excludeBias <<- excludeBias
  neuralNetwork.hiddenLayers <<- hiddenLayers
  data.trainSets.ids <- names(data.trainSets)
  trainSetsCombined <- rbindlist(data.trainSets)
  numNN <- 2 * length(data.trainSets.ids) + 2
  
  
  withProgress(message = "Learning Neural Network", value = 0, {
  # Learn neural networks without a hidden layer
    for (i in 1:length(data.trainSets.ids))
    {
      id <- data.trainSets.ids[i]
      
      incProgress(1/numNN, detail = paste('Without hidden layers: ID', id))
      getNeuralNetwork(id)
    }
    
    incProgress(1/numNN, detail = 'Without hidden layers: General for all')
    getNeuralNetwork(NULL)
    
    # Learn neural networks with hidden layer
    for (i in 1:length(data.trainSets.ids))
    {
      id <- data.trainSets.ids[i]
      
      incProgress(1/numNN, detail = paste('With hidden layers: ID', id))
      getNeuralNetwork(id, hiddenLayers = TRUE)
    }
    
    incProgress(1/numNN, detail = 'With hidden layers: General for all')
    getNeuralNetwork(NULL, hiddenLayers = TRUE)
  })
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

# Get test result of the neural network for the given parameters
getNeuralNetworkTestResults <- function(id, forAll = FALSE, hiddenLayers = FALSE) {
  if (forAll)
  {
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.testResults.forAll.hiddenLayers[[id]]))
      {
        neuralNetwork.testResults.forAll.hiddenLayers[[id]] <<- testNeuralNetwork(getNeuralNetwork(NULL, TRUE), id)
      }
      
      return(neuralNetwork.testResults.forAll.hiddenLayers[[id]])
    }
    else
    {
      if (is.null(neuralNetwork.testResults.forAll[[id]]))
      {
        neuralNetwork.testResults.forAll[[id]] <<- testNeuralNetwork(getNeuralNetwork(NULL), id)
      }
      
      return(neuralNetwork.testResults.forAll[[id]])
    }
  }
  else
  {
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.testResults.forEach.hiddenLayers[[id]]))
      {
        neuralNetwork.testResults.forEach.hiddenLayers[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, TRUE), id)
      }
      
      return(neuralNetwork.testResults.forEach.hiddenLayers[[id]])
    }
    else
    {
      if (is.null(neuralNetwork.testResults.forEach[[id]]))
      {
        neuralNetwork.testResults.forEach[[id]] <<- testNeuralNetwork(getNeuralNetwork(id), id)
      }
      
      return(neuralNetwork.testResults.forEach[[id]])
    }
  }
}

testNeuralNetworks <- function() {
  neuralNetwork.forEach.ids <- names(neuralNetwork.forEach)
  for (i in 1:length(neuralNetwork.forEach.ids))
  {
    id <- neuralNetwork.forEach.ids[i]
    
    getNeuralNetworkTestResults(id)
    getNeuralNetworkTestResults(id, hiddenLayers = TRUE)
    getNeuralNetworkTestResults(id, forAll = TRUE)
    getNeuralNetworkTestResults(id, forAll = TRUE, hiddenLayers = TRUE)
  }
}
