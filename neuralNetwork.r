library(neuralnet)

neuralNetwork.excludeBias <<- TRUE
neuralNetwork.hiddenLayers <<- c(0)
neuralNetwork.excludeVector <<- NULL

neuralNetwork.enableForEach <<- TRUE
neuralNetwork.enableForEach.hidden <<- TRUE
neuralNetwork.enableForAll <<- FALSE
neuralNetwork.enableForAll.hidden <<- FALSE

neuralNetwork.enableHiddenSandbox <<- FALSE
neuralNetwork.hiddenLayersOptimization <<- c(0)

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
  resetNeuralNetworks.hiddenSandbox()
  
  neuralNetwork.forEach <<- NULL
  neuralNetwork.forAll <<- NULL
  
  neuralNetwork.testResults.forEach <<- NULL
  neuralNetwork.testResults.forAll <<- NULL
}

resetNeuralNetworks.hiddenSandbox <- function()
{
  neuralNetwork.hiddenSandbox <<- NULL
  neuralNetwork.testResults.hiddenSandbox <<- NULL
}

setNeuralNetworkExcludeVector <- function() {
  # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
  neuralNetwork.excludeVector <<- c(1) # first bias needs to be excluded every time
  if(neuralNetwork.hiddenLayers[1] > 0){ #only if hidden layer are present
    
    layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hiddenLayers)...))
    layer_vector[1] <- data.windowSize #number of input neurons
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
  set.seed(1)
  if(neuralNetwork.excludeBias) {

    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity,
      exclude = (if(hiddenLayers == c(0)) c(1) else neuralNetwork.excludeVector))
  }
  else {
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity)
  }
}

# Get the neural network for the given parameters (one neural network for all if is.null(id))
# Compute the neural network if necessary
getNeuralNetwork <- function(id, hiddenLayers = FALSE, sandbox = FALSE) {
  if(sandbox)
  {
    neuralNetwork.hiddenSandbox <<- optimizeNeuralNetworkHiddenLayer(id, hiddenLayers, sandbox)
  }
  if (is.null(id))
  {
    trainSetsCombined <- getAllTrainSetsCombined()
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.forAll.hiddenLayers))
      {
        print('train for all with hidden layers')
        neuralNetwork.forAll.hiddenLayers <<- trainNeuralNetwork(trainSetsCombined, neuralNetwork.hiddenLayers)
      }
      
      return(neuralNetwork.forAll.hiddenLayers)
    }
    else
    {
      if (is.null(neuralNetwork.forAll))
      {
        print('train for all without hidden layers')
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
        print(paste('train for id', id ,'with hidden layers'))
        neuralNetwork.forEach.hiddenLayers[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hiddenLayers)
      }
      
      return(neuralNetwork.forEach.hiddenLayers[[id]])
    }
    else
    {
      if (is.null(neuralNetwork.forEach[[id]]))
      {
        print(paste('train for id', id ,'without hidden layers'))
        neuralNetwork.forEach[[id]] <<- trainNeuralNetwork(getTrainSet(id))
      }
      
      return(neuralNetwork.forEach[[id]])
    }
  }
}

testNeuralNetwork <- function(neuralNetwork, testSetID) {
  expected <- getTestSet(testSetID)$xt0
  testData <- getTestSet(testSetID)
  testData$xt0 <- NULL
  
  n <- compute(neuralNetwork, testData)
  n$net.expected <- expected

  n$net.mse <- sum((n$net.expected - n$net.result)^2)/nrow(n$net.result)
  
  n
}

# Get test result of the neural network for the given parameters
getNeuralNetworkTestResults <- function(id, forAll = FALSE, hiddenLayers = FALSE, sandbox = FALSE) {
  if(sandbox)
  {
    if (is.null(neuralNetwork.testResults.hiddenSandbox[[id]]))
      {
        neuralNetwork.testResults.hiddenSandbox[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hiddenLayers = TRUE, sandbox = TRUE), id)
      }
      
      return(neuralNetwork.testResults.hiddenSandbox[[id]])
  }
  if (forAll)
  {
    if (hiddenLayers)
    {
      if (is.null(neuralNetwork.testResults.forAll.hiddenLayers[[id]]))
      {
        neuralNetwork.testResults.forAll.hiddenLayers[[id]] <<- testNeuralNetwork(getNeuralNetwork(NULL, hiddenLayers = TRUE), id)
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
        neuralNetwork.testResults.forEach.hiddenLayers[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hiddenLayers = TRUE), id)
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

findDifferenceInNeuralNetworksWrtHiddenLayers <- function() {
  ids <- names(data.trainSets)
  numHiddenNeurons <- neuralNetwork.hiddenLayers[1]
  tolerance <- 1e-5
  
  idsNNsDiffer <- NULL
  
  for (i in 1:length(ids))
  {
    id <- ids[i]
    
    nn <- getNeuralNetwork(id)
    nnh <- getNeuralNetwork(id, TRUE)
    
    for (j in 1:data.windowSize)
    {
      sum <- 0
      for (k in 1:numHiddenNeurons)
      {
        sum <- sum + nnh$weights[[1]][[1]][j+1, k] * nnh$weights[[1]][[2]][k+1, 1]
      }
      
      if (abs(sum - nn$weights[[1]][[1]][j+1, 1]) > tolerance)
      {
        idsNNsDiffer <- c(idsNNsDiffer, id)
        print(paste('difference', id, j, sum, nn$weights[[1]][[1]][j+1, 1]))
        break
      }
    }
  }
  
  data.table(ids = idsNNsDiffer)
}

getNeuralNetworkPredictionPlotly <- function(id, forAll = FALSE, hiddenLayers = FALSE, sandbox = FALSE) {
  if (is.null(id))
  {
    return(NULL)
  }
  
  testResults <- getNeuralNetworkTestResults(id, forAll, hiddenLayers, sandbox)
  if (is.null(testResults))
  {
    return(NULL)
  }
  
  data.length <- length(data.sets[[id]]$y)
  numTestResults <- length(testResults$net.result)
  startRealData <- max(1, data.length - 2 * numTestResults + 1)
  
  prediction <- rbindlist(list(
    as.data.table(rep(NA, numTestResults)),
    as.data.table(testResults$net.result)
  ))
  names(prediction) <- c('prediction')
  
  prediction$x <- data.sets[[id]]$x[startRealData:data.length]
  prediction$y <- data.sets[[id]]$y[startRealData:data.length]
  
  startIndex = data.length - startRealData - numTestResults + 1
  prediction$prediction[[startIndex]] <- prediction$y[[startIndex]]
  
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original') %>%
    add_trace(y = ~prediction, name = 'Prediction', line = list(dash = 'dash'))
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

optimizeNeuralNetworkHiddenLayer <- function(id, hiddenLayers = FALSE, sandbox = FALSE)
{
  if (is.null(neuralNetwork.hiddenSandbox[[id]]))
    {
      neuralNetwork.hiddenSandbox[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hiddenLayersOptimization)
      last_error <- neuralNetwork.hiddenSandbox[[id]]$result.matrix["error", 1]
      print(data.windowSize)
      for(i in 1:length(data.windowSize)){
        cat("hidden layer vector", c(i))
        neuralNetwork.hiddenLayersOptimization <<- c(i)
        neuralNetwork.hiddenSandbox[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hiddenLayersOptimization)
        current_error <- neuralNetwork.hiddenSandbox[[id]]$result.matrix["error", 1]

        if(last_error > current_error)
        {
          cat("current error", current_error)
          cat("last error", last_error)
          print("found local minimum for hidden layer optimization")
          #return(neuralNetwork.hiddenSandbox[[id]])
        }
        #TODO abbruchbedingung
      }
    }      
    print("no local optimisation")
    return(neuralNetwork.hiddenSandbox[[id]])
}