library(neuralnet)

neuralNetwork.excludeBias <<- TRUE
neuralNetwork.hiddenLayers <<- c(0)
neuralNetwork.excludeVector <<- NULL
neuralnetwork.isInputExcluded <<- FALSE
neuralNetwork.excludedMaxInputs <<- 0
neuralNetwork.excludedInput <<- 0
neuralNetwork.excludedPastError <<- -1
neuralNetwork.excludedPastModel <<- NULL

neuralNetwork.enableForEach <<- TRUE
neuralNetwork.enableForEach.hidden <<- TRUE
neuralNetwork.enableForAll <<- FALSE
neuralNetwork.enableForAll.hidden <<- FALSE

neuralNetwork.hlOptimization <<- c(0)
neuralNetwork.hlOptimizationErrorVector <<- c(0)
neuralnetwork.tempHiddenNodes <<- c(0)

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
  resetNeuralNetworks.hlOptimizationNN()
  
  neuralNetwork.forEach <<- NULL
  neuralNetwork.forAll <<- NULL
  
  neuralNetwork.testResults.forEach <<- NULL
  neuralNetwork.testResults.forAll <<- NULL
  
  
}

resetNeuralNetworks.hlOptimizationNN <- function()
{
  neuralNetwork.hlOptimizationNN <<- NULL
  neuralNetwork.hlOptimizationNN.old <<- NULL
  neuralNetwork.testResults.hlOptimizationNN<<- NULL
  neuralNetwork.testResults.hlOptimizationNN.old <<- NULL
}

setNeuralNetworkExcludeVector <- function(hidden, excludeInputNodes = c(0)) {
  # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
  
  neuralNetwork.excludeVector <<- vector()
  
  if(neuralNetwork.excludeBias)
  {
    neuralNetwork.excludeVector <<- c(1) # first bias needs to be excluded every time
  }
  
  if((neuralnetwork.isInputExcluded && !(neuralNetwork.excludedInput == neuralNetwork.excludedMaxInputs)) || excludeInputNodes != 0)
  {
    neuralNetwork.excludeVector <<- c(neuralNetwork.excludeVector, getMinimalWeightVector(hidden, excludeInputNodes))
  }
  
  if(hidden[1] > 0 && neuralNetwork.excludeBias){ #only if hidden layer are present
    
    layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hiddenLayers)...))
    layer_vector[1] <- data.windowSize #number of input neurons
    for(i in 1 : length(neuralNetwork.hiddenLayers))
    {
      layer_vector[i + 1] <- neuralNetwork.hiddenLayers[i] #create the layer vector
    }
    current_bias <- 1 #first bias always 1
    exclude_counter <- length(neuralNetwork.excludeVector) + 1 #to fill the exclude-vector. first one is already filled (1), so begin with 2
    for (i in 1 : length(neuralNetwork.hiddenLayers)){ #iterate through the vertical layers, beginning with input nodes
      for (j in 1 : neuralNetwork.hiddenLayers[i]){ #iterate through the nodes of the current layer
        current_bias <- current_bias + layer_vector[i] + 1 #from bias to bias calculate new_value = old_value + n + 1 where n is number of nodes in current layer
        neuralNetwork.excludeVector[exclude_counter] <<- current_bias #add to vector for excluded weights
        exclude_counter <- exclude_counter + 1 #go to next element in exclude vector
      }
    }
  }
  neuralNetwork.excludeVector <<- sort(neuralNetwork.excludeVector, decreasing = FALSE)
  
}

getMinimalWeightVector <- function(hidden, excludeInputNode = c(0))
{
  ws <- vector()
  vec <- vector()
  
  if(hidden != 0)
  {
    hidden    <- neuralNetwork.hiddenLayers
    sumVector <- vector()
    idx       <- 0
    
    if(excludeInputNode == 0)
    {
      ws <- abs(neuralNetwork.excludedPastModel$weights[[1]][[1]][1 : (data.windowSize * hidden + (hidden))])
      
      for(j in 1 : data.windowSize)
      {
        s <- vector()
        for(i in 1 : hidden)
        {
          from <- ((i - 1) * (data.windowSize + 1)) + 1
          s <- abs(c(s, ws[from + j ])) # first point + bias + input offset
        }
        sumVector[j] <- sum(s, na.rm = TRUE)
      }
      idx <- c(vec,which(sumVector == 0))
      sumVector <- replace(sumVector, sumVector==0, NaN)
      idx <- c(idx, which(sumVector == min(sumVector, na.rm = TRUE)))
      
    }
    else
    {
      idx <- excludeInputNode
    }
    
    
    for(id in idx)
    {
      for(i in 1 : hidden)
      {
        from <- ((i - 1) * (data.windowSize + 1)) + 1
        vec <- c(vec, from + id)
      }
    }
    
  }
  else
  {
    if(excludeInputNode == 0)
    {
      ws <- abs(neuralNetwork.excludedPastModel$weights[[1]][[1]][2 : (data.windowSize + 1)]) # get weight input vector without bias
      vec <- which(is.na(ws)) + 1 # add bias offset
      vec[(length(vec) + 1)] = which(ws == min(ws, na.rm = TRUE)) + 1 # add bias offset
    }
    else
    { 
      vec[1] = excludeInputNode + 1
    }
  }
  return(vec)
}




trainNeuralNetwork <- function(trainset, hiddenLayers = c(0), excludeInputNode = c(0)) 
{
  
  setNeuralNetworkExcludeVector(hiddenLayers, excludeInputNode)
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  set.seed(1)

  
  if(!is.logical(neuralNetwork.excludeVector) || neuralNetwork.excludeBias) {
    nn <- 1
    out <- tryCatch({
      nn <- neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity,
      exclude = neuralNetwork.excludeVector)
      return(nn)
    },
      warning=function(cond) {
      message(paste("Warning:"))
      nn <- -1
      return(nn)
    })
    
    return(out)
  }
  else
  {
    neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity)
  }
}

# Get the neural network for the given parameters (one neural network for all if is.null(id))
# Compute the neural network if necessary
getNeuralNetwork <- function(id, hiddenLayers = FALSE, hlOptimization = FALSE) {
  #trycatch
  
  out <- tryCatch({
  if(hlOptimization) #when optimal number of hidden nodes must be retrieved
  {
    if(is.null(neuralNetwork.hlOptimizationNN[[id]]))
    {
      neuralNetwork.hlOptimizationNN[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hlOptimization)    
      return(neuralNetwork.hlOptimizationNN[[id]])
    } else {
      neuralNetwork.hlOptimizationNN.old[[id]] <<- neuralNetwork.hlOptimizationNN[[id]]
      neuralNetwork.hlOptimizationNN[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hlOptimization)    
      return(neuralNetwork.hlOptimizationNN[[id]])
    }
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
        if(changedExcludedInput(neuralNetwork.forAll.hiddenLayers))
        {
          neuralNetwork.forAll.hiddenLayers <<- NULL
          getNeuralNetwork(id, hiddenLayers)
        }
      }
      if(neuralnetwork.isInputExcluded) neuralNetwork.forAll.hiddenLayers <<- neuralNetwork.excludedPastModel
      return(neuralNetwork.forAll.hiddenLayers)
    }
    else
    {
      if (is.null(neuralNetwork.forAll))
      {
        print('train for all without hidden layers')
        neuralNetwork.forAll <<- trainNeuralNetwork(trainSetsCombined)
        if(changedExcludedInput(neuralNetwork.forAll))
        {
          neuralNetwork.forAll <<- NULL
          getNeuralNetwork(id, hiddenLayers)
        }
      }
      if(neuralnetwork.isInputExcluded) neuralNetwork.forAll <<- neuralNetwork.excludedPastModel
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
        if(changedExcludedInput(neuralNetwork.forEach.hiddenLayers[[id]]))
        {
          neuralNetwork.forEach.hiddenLayers[[id]] <<- NULL
          getNeuralNetwork(id, hiddenLayers)
        }
      }
      if(neuralnetwork.isInputExcluded) neuralNetwork.forEach.hiddenLayers[[id]] <<- neuralNetwork.excludedPastModel
      return(neuralNetwork.forEach.hiddenLayers[[id]])
    }
    else
    {
      if (is.null(neuralNetwork.forEach[[id]]))
      {
        print(paste('train for id', id ,'without hidden layers'))
        neuralNetwork.forEach[[id]] <<- trainNeuralNetwork(getTrainSet(id))
        if(changedExcludedInput(neuralNetwork.forEach[[id]]))
        {
          neuralNetwork.forEach[[id]] <<- NULL
          getNeuralNetwork(id, hiddenLayers)
        }
      }
      if(neuralnetwork.isInputExcluded)  neuralNetwork.forEach[[id]] <<- neuralNetwork.excludedPastModel
      return(neuralNetwork.forEach[[id]])
    }
  }
  }, 
#  error=function(cond) {
#    message(paste("Error:", id))
#    message("Here's the original error message:")
#    message(cond)
#    return(NA)
#  },
  warning=function(cond) {
   
    message(paste("Warning:", id, hiddenLayers))
    message("Here's the original warning message:")
    message(cond)
    return(NULL)
  }
  )    
  return(out)
}

changedExcludedInput <- function(model)
{
  if(neuralnetwork.isInputExcluded == FALSE) return(FALSE)
  
  error = testNeuralNetwork(model, data.idSelected)$mse
  print(error)
  print(model$weights)
  
  
  if((neuralNetwork.excludedPastError < 0) || (neuralNetwork.excludedInput > 0 && error <= (neuralNetwork.excludedPastError)))
  {
    neuralNetwork.excludedPastError <<- error
    neuralNetwork.excludedPastModel <<- model
    neuralNetwork.excludedInput <<- neuralNetwork.excludedInput - 1
    return(TRUE)
  }
  
  neuralNetwork.excludedPastError <<- - 1
  neuralNetwork.excludedInput <<- neuralNetwork.excludedMaxInputs
  return(FALSE)
}


# it eliminates every input one times and shows error in a matrix -> x - which input Node was eliminated, y - which error has the model
getNeuralNetworkInputErrorTable <- function(id, hiddenLayers = FALSE)
{
  
  eM <- matrix(nrow = data.windowSize + 1, ncol = 2) # plus 1 for the original NN
  for(i in 1 : (data.windowSize + 1))
  {
    j <- i - 1 
    eM[i,1] = j
    
      if (is.null(id))
      {
        trainSetsCombined <- getAllTrainSetsCombined()
        if (hiddenLayers)
        {
          eM[i,2] <- testNeuralNetwork(trainNeuralNetwork(trainSetsCombined, neuralNetwork.hiddenLayers, j), data.idSelected)$mse 
        }
        else
        {
          eM[i,2] <-  testNeuralNetwork(trainNeuralNetwork(trainSetsCombined, c(0), j), data.idSelected)$mse 
        }
      }
      else
      {
        if (hiddenLayers)
        {
          eM[i,2] <-  testNeuralNetwork(trainNeuralNetwork(getTrainSet(id), neuralNetwork.hiddenLayers, j), id)$mse 
        }
        else
        {
          eM[i,2] <-  testNeuralNetwork(trainNeuralNetwork(getTrainSet(id), c(0), j), id)$mse 
          
        }
      }
  }
  
  return(data.table(inputNode = eM[,1], "mse error" = eM[,2]))
}



testNeuralNetwork <- function(neuralNetwork, testSetID)
{
  testData <- getTestSet(testSetID)
  expected <- testData$xt0
  testData$xt0 <- NULL
  
  #compute = boese
  n <- compute(neuralNetwork, testData)
  
  mse<- sum((expected - n$net.result)^2)/nrow(n$net.result)
  structure(list(expected = expected, result = n$net.result, mse = mse), class = 'TestResults')
}

# Get test result of the neural network for the given parameters
getNeuralNetworkTestResults <- function(id, forAll = FALSE, hiddenLayers = FALSE) {  
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

getReducedNeuralNetworkWeights <- function(nn) {
  lapply(nn$weights, function(repetition) {
    repetition <- lapply(repetition, function(layer) {
      # Change all NA weights to 0
      matrixData <- unlist(lapply(layer, function(weight) {
        if (is.na(weight))
        {
          return (0)
        }
        else
        {
          return (weight)
        }
      }))
      
      # Extend the matrix data with a bias column to be able to use simple matrix multiplication
      matrixData <- c(1, rep(0, NROW(layer) - 1), matrixData)
      
      matrix(matrixData, ncol = NCOL(layer) + 1)
    })
    
    while (length(repetition) > 1)
    {
      repetition[[1]] <- repetition[[1]] %*% repetition[[2]]
      repetition[[2]] <- NULL
    }
      
    # Remove added bias column
    list(matrix(repetition[[1]][, -1], ncol = NCOL(repetition[[1]]) - 1))
  })
}

findDifferenceInNeuralNetworksWrtHiddenLayers <- function() {
  tolerance <- 1e-5
  
  idsNNsDiffer <- unlist(lapply(names(data.sets), function(id) {
    reducedWeightsNN <- getReducedNeuralNetworkWeights(getNeuralNetwork(id))[[1]][[1]][,1]
    reducedWeightsNNH <- getReducedNeuralNetworkWeights(getNeuralNetwork(id, TRUE))[[1]][[1]][,1]
    
    dif <- reducedWeightsNN - reducedWeightsNNH
    
    for (i in 1:length(dif))
    {
      if (abs(dif[i]) > tolerance)
      {
        return (id)
      }
    }
    
    return (NULL)
  }))
  
  if (length(idsNNsDiffer) > 0)
  {
    data.table(ids = idsNNsDiffer)
  }
  else
  {
    NULL
  }
}

optimizeNeuralNetworkHiddenLayer <- function(id)
{
  resetNeuralNetworks.hlOptimizationNN()
  neuralNetwork.hlOptimizationErrorVector <<- c(0)
  neuralNetwork.tempHiddenNodes <<- neuralNetwork.hiddenLayers
  if (is.null(neuralNetwork.hlOptimizationNN[[id]]))
  {
    #get neural network and error without hidden layer
    neuralNetwork.hlOptimization <<- c(0)
    neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
    neuralNetwork.testResults.hlOptimizationNN.old[[id]] <<- neuralNetwork.testResults.hlOptimizationNN[[id]]     

    last_error <- neuralNetwork.testResults.hlOptimizationNN[[id]]$mse
    neuralNetwork.hlOptimizationErrorVector[1] <<- last_error 
    
    #add first layer incrementally
    for (i in 1:data.windowSize){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(i)
      neuralNetwork.hiddenLayers <<- c(i)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- neuralNetwork.testResults.hlOptimizationNN[[id]]$mse
      neuralNetwork.hlOptimizationErrorVector[i+1] <<- current_error

      #break from optimization when error rises again
      if(last_error < current_error)
      {
        neuralNetwork.hlOptimization <<- c(i-1)
        #return(neuralNetwork.hlOptimization)
      }
      #set new error to last error and save previous neural network
      last_error <- current_error
      neuralNetwork.testResults.hlOptimizationNN.old[[id]] <<- neuralNetwork.testResults.hlOptimizationNN[[id]]
      neuralNetwork.hlOptimization
    }

    m <- data.windowSize
    #add first layer incrementally
    for (j in 1:m){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(m, j)
      neuralNetwork.hiddenLayers <<- c(m, j)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- neuralNetwork.testResults.hlOptimizationNN[[id]]$mse
      neuralNetwork.hlOptimizationErrorVector[m+j+1] <<- current_error

      #break from optimization when error rises again
      if(last_error < current_error)
      {
        neuralNetwork.hlOptimization <<- c(m, j)
        neuralNetwork.hiddenLayers <<- c(m, j)
        #return(neuralNetwork.hlOptimization)
      }
      #set new error to last error and save previous neural network
      last_error <- current_error
      neuralNetwork.testResults.hlOptimizationNN.old[[id]] <<- neuralNetwork.testResults.hlOptimizationNN[[id]]
      neuralNetwork.hlOptimization
    }
  }   
}

getHlOptimizationErrorTable <- function(id)
{
  #print(neuralNetwork.hlOptimizationErrorVector)
  #return(as.data.table(neuralNetwork.hlOptimizationErrorVector))

  row <- (length(neuralNetwork.hlOptimizationErrorVector))

  eM <- matrix(nrow = row, ncol = 3) # plus 1 for the original NN
  n_firstlayer <- neuralNetwork.hlOptimization[1]
  n_seclayer <- neuralNetwork.hlOptimization[2]


  for(i in 1 : row)
  {
    eM[i,3] <- neuralNetwork.hlOptimizationErrorVector[i]
  }

  first_index <- n_firstlayer + 1
  eM[1,1] <- 0
  eM[1,2] <- 0
  for(k in 1 : first_index)
  {
    eM[k+1,1] <- k
    eM[k+1,2] <- 0
  }

  second_index <- n_seclayer
  for(l in 1 : second_index)
  {
    offset <- first_index + l
    eM[offset, 1] <- n_firstlayer
    eM[offset, 2] <- l
  }
  
  return(data.table("first layer" = eM[,1], "second layer" = eM[,2], "mse" = eM[,3]))
}