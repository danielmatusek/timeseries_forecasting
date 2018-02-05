library(neuralnet)


neuralnetwork.strategies         <<- c("Greedy") 
neuralnetwork.greedyErrorType     <<- 'Outsample'

neuralNetwork.hlOptimization <<- c(0)
neuralNetwork.hlOptimizationErrorVector <<- c(0)
neuralnetwork.tempHiddenNodes <<- c(0)

resetNeuralNetworks.hidden <- function()
{
  resetModels('nnfeh')
  resetModels('nnfah')
  resetModels('nnfehd')
}

resetNeuralNetworks <- function()
{
  resetNeuralNetworks.hidden()
  resetNeuralNetworks.hlOptimizationNN()
  
  resetModels('nnfe')
  resetModels('nnfa')
  resetModels('nnfeei')
  resetModels('nnfed')
  resetModels('nnfamei')
}


resetNeuralNetworks.InputExclusion <- function()
{
  neuralNetwork.excludedInputNodes <<- list()
  neuralNetwork.excludedPastModels <<- list()
  neuralNetwork.excludedPastErrors <<- vector()
  neuralNetwork.excludedInternalErrors <<- vector()
}


resetNeuralNetworks.hlOptimizationNN <- function()
{
  neuralNetwork.hlOptimizationNN <<- NULL
  neuralNetwork.hlOptimizationNN.old <<- NULL
  neuralNetwork.testResults.hlOptimizationNN<<- NULL
  neuralNetwork.testResults.hlOptimizationNN.old <<- NULL
}

getNeuralNetworkExcludeVector <- function(hidden, excludeVector = NULL, isDifferantable = FALSE) {
  # Calculate Bias Neuron weights (see: https://stackoverflow.com/q/40633567)
  
  excludedVectorPos <- vector()
  
  #exclude first bias
  if(vars$options$excludeBias)
  {
    excludedVectorPos <- c(1) # first bias needs to be excluded every time
  }
  
  #exclude Inputs
  if(!is.null(excludeVector))
  {
    excludedVectorPos <- c(excludedVectorPos, excludeInputs(hidden, excludeVector))
  }
  
  #exclude bias with hidden layers
  if(hidden[1] > 0 && vars$options$excludeBias){ #only if hidden layer are present
    
    layer_vector <- NULL #we need the number of nodes in every vertical row (e.g. c(num(input_neuron), num(first_hiddenLayers)...))
    layer_vector[1] <- if(isDifferantable){2 * vars$options$windowSize - 1}else{vars$options$windowSize} #number of input neurons
    for(i in 1 : length(vars$options$hiddenLayers))
    {
      layer_vector[i + 1] <- vars$options$hiddenLayers[i] #create the layer vector
    }
    current_bias <- 1 #first bias always 1
    exclude_counter <- length(excludedVectorPos) + 1 #to fill the exclude-vector. first one is already filled (1), so begin with 2
    for (i in 1 : length(vars$options$hiddenLayers)){ #iterate through the vertical layers, beginning with input nodes
      for (j in 1 : vars$options$hiddenLayers[i]){ #iterate through the nodes of the current layer
        current_bias <- current_bias + layer_vector[i] + 1 #from bias to bias calculate new_value = old_value + n + 1 where n is number of nodes in current layer
        excludedVectorPos[exclude_counter] <- current_bias #add to vector for excluded weights
        exclude_counter <- exclude_counter + 1 #go to next element in exclude vector
      }
    }
  }
  excludedVectorPos <- sort(excludedVectorPos, decreasing = FALSE)
}

# def: exclude the input nodes
# param1 : is hidden?
# param2 : vector of positions which should be excluded
excludeInputs <- function(hidden, excludeVector)
{
  vec <- vector()
  
  if(is.null(excludeVector)) return(NULL)
  
  if(hidden)
  {
    for(pos in excludeVector)
    {
      for(i in 1 : vars$options$hiddenLayers)
      {
        from <- ((i - 1) * (vars$options$windowSize + 1)) + 1
        vec <- c(vec, from + pos)
      }
    }
  }
  else
  {
    vec = excludeVector + 1
  }
  return(vec)
}




trainNeuralNetwork <- function(trainset, hiddenLayers = c(0), excludeVector = NULL, isDifferantable = FALSE) 
{
  
  excludedVectorPos <- getNeuralNetworkExcludeVector(hiddenLayers, excludeVector, isDifferantable)
  
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  set.seed(1)
  
  
  tryCatch({
    #if elements exist in excludevector then the condition is false
    if(!is.logical(excludedVectorPos))
    {
      nn <- neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity,
        exclude = excludedVectorPos)
    }
    else
    {
      nn <- neuralnet(f, trainset, hidden = hiddenLayers, linear.output = TRUE, act.fct = identity)
    }
    
    # remove "unnecessary" fields. They fill up RAM.
    nn$response <- NULL
    nn$coveriate <- NULL
    nn$data <- NULL
    nn$covariate <- NULL
    nn$exclude <- NULL
    nn$call <- NULL
    nn$err.fct <- NULL
    nn$net.result <- NULL
    nn$startweights <- NULL
    nn$generalized.weights <- NULL
    nn
  },
  warning = function(w) {
    print(w)
    NA
  },
  error = function(e) {
    print(e)
    NA
  })
}

getModel.nnfe <- function(id)
{
  trainNeuralNetwork(getTrainSet(id))
}

getModel.nnfeh <- function(id)
{
  trainNeuralNetwork(getTrainSet(id), vars$options$hiddenLayers)
}

getModel.nnfa <- function()
{
  trainSetsCombined <- getAllTrainSetsCombined()
  trainNeuralNetwork(trainSetsCombined)
}

getModel.nnfah <- function()
{
  trainSetsCombined <- getAllTrainSetsCombined()
  trainNeuralNetwork(trainSetsCombined, vars$options$hiddenLayers)
}


# ------ Excluded Input Models
getModel.nnfeei <- function(id)
{
  getExcludedInputNeuralNetwork(id, FALSE)
}


# -----Differentiable Input Models
getModel.nnfed <- function(id)
{
  trainNeuralNetwork(getDiffTrainSet(id),FALSE, NULL, TRUE)
}


# ---- Excluded Input Statistics
getModel.nnfamei <- function()
{
  getStatisticOfExcludedInputs()
}



# Get the neural network for the given parameters (one neural network for all if is.null(id))
# Compute the neural network if necessary
getNeuralNetwork <- function(id, hiddenLayers = FALSE, hlOptimization = FALSE) {
  if(hlOptimization) #when optimal number of hidden nodes must be retrieved
  {
    if(is.null(neuralNetwork.hlOptimizationNN[[id]]))
    {
      neuralNetwork.hlOptimizationNN[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hlOptimization)    
      return(neuralNetwork.hlOptimizationNN[[id]])
    }
    else
    {
      neuralNetwork.hlOptimizationNN.old[[id]] <<- neuralNetwork.hlOptimizationNN[[id]]
      neuralNetwork.hlOptimizationNN[[id]] <<- trainNeuralNetwork(getTrainSet(id), neuralNetwork.hlOptimization)    
      return(neuralNetwork.hlOptimizationNN[[id]])
    }
  }
}


testNeuralNetwork <- function(neuralNetwork, testSetID, isDiffInput = FALSE)
{
  testData <- if(isDiffInput){getDiffTestSet(testSetID)[,-1]}else{getTestSet(testSetID)}
  compute(neuralNetwork, testData)$net.result[,1]
}

getTestResults.nnfe <- function(model, id)
{
  testNeuralNetwork(model, id)
}

getTestResults.nnfeh <- function(model, id)
{
  testNeuralNetwork(model, id)
}

getTestResults.nnfa <- function(model, id)
{
  testNeuralNetwork(model, id)
}

getTestResults.nnfah <- function(id)
{
  testNeuralNetwork(model, id)
}

# test excluded Input
getTestResults.nnfeei <- function(model, id)
{

  testNeuralNetwork(model, id)
}

# test differntable Input
getTestResults.nnfed <- function(model, id)
{
  testNeuralNetwork(model, id, TRUE)
}


getTestResults.nnfamei <- function(model, id)
{
  testNeuralNetwork(model, id)
}


getReducedNeuralNetworkWeights <- function(nn) {
  lapply(nn$weights, function(repetition) {
    repetition <- lapply(repetition, function(layer) {
      # Change all NA weights to 0
      layer[is.na(layer)] <- 0
      
      # Extend the matrix data with a bias column to be able to use simple matrix multiplication
      matrixData <- c(1, rep(0, NROW(layer) - 1), layer[,1])
      
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



getExcludedInputNeuralNetwork <- function(id, hiddenLayers = FALSE)
{
  baseModelName <- if(hiddenLayers) { if(is.null(id)) { 'nnfah' } else { 'nnfeh' }} else { if(is.null(id)) { 'nnfa' } else { 'nnfe'}}
  internalError = list()
  externalError = list()
  excludedPath <- NULL
  bestModel = getModel(baseModelName, id)
  
  if (mode(bestModel) == 'logical')
  {
    internalError[['empty']] <- NA
    externalError[['empty']] <- NA
    bestExclInput <- NULL
    bestError <- .Machine$double.xmax
  }
  else
  {
    testResults <- getTestResults(baseModelName, id)
    smape <- sMAPE(testResults$expected, testResults$predicted)
    externalError[['empty']] <- if(!is.nan(smape)){ smape }else{ 0 }
    internalError[['empty']] <- bestModel$result.matrix[1]
    bestExclInput <- 'empty'
    bestError <- if(neuralnetwork.greedyErrorType == 'Outsample') {externalError[['empty']]} else {internalError[['empty']]}
  }
  
  queue <- lapply(1 : vars$options$windowSize, function(i) { i })
  
  trainSet <- getTrainSet(id)
  
  repeat
  {
    newBestInput <- NULL
    while(length(queue) > 0)
    {
      path <- queue[[1]]
      
      if(length(path) == vars$options$windowSize) break
      
      nn <- trainNeuralNetwork(trainSet, hiddenLayers, path)
      pathAsString <- paste(path, collapse = ",")

      if (mode(nn) == 'logical')
      {
        internalError[[pathAsString]] <- NA
        externalError[[pathAsString]] <- NA
      }
      else
      {
        testResults <- testNeuralNetwork(nn, id)
        smape <- sMAPE(data.expectedTestResults[[id]], testResults)
        externalError[[pathAsString]] <-  if(!is.nan(smape)){ smape }else{ 0 }
        internalError[[pathAsString]] <- nn$result.matrix[1]
        
        error <- if(neuralnetwork.greedyErrorType == 'Outsample') {externalError[[pathAsString]]} else {internalError[[pathAsString]]}
        if (error < bestError)
        {
          bestModel <- nn
          newBestInput <- path
          excludedPath <- path
          bestError <- if(neuralnetwork.greedyErrorType == "Insample"){internalError[[pathAsString]]} else{externalError[[pathAsString]]}
        }
      }
      
      if(length(queue) == 1)
      {
        queue <- NULL
        break
      }
      
      queue <- queue[-1]
    }
    
    if (is.null(newBestInput))
    {
      break
    }
    else
    {
      newInputs <- setdiff(1 : vars$options$windowSize, newBestInput)
      queue <- lapply(newInputs, function(newInput) { c(newBestInput, newInput) })
    }
  }
  
  
  if (mode(bestModel) != 'logical')
  {
    bestModel$inSampleError <- internalError
    bestModel$outSampleError <- externalError
    bestModel$path <- excludedPath
  }
  else
  {
    bestModel <- NA
  }
  
  return(bestModel)
}



getStatisticOfExcludedInputs <-function(hiddenLayers = FALSE)
{
  ids <- names(vars$timeSeries)
  numOfExclusionPerNode <- c(rep(0,vars$options$windowSize))

  for(id in ids)
  {
    model <- if(!hiddenLayers){getModel('nnfeei',id)}else{getModel('nnfehei',id)}
    if(mode(model) != 'logic')
    {
      weights <- model$weights[[1]][[1]][2 : length(model$weights[[1]][[1]][,1]),1]
      for(i in which(is.na(weights))) 
      {
        numOfExclusionPerNode[i] <- numOfExclusionPerNode[i] + 1
      }
    }
  }
  
  excludedInputs <- unlist(lapply(ids, function(id) {
  model <- getModel('nnfeei', id)
  paths <- NULL
  if(!is.null(model$path)) paths <-  sort(model$path, decreasing = FALSE)
  
  paste(paths, collapse=",")
  }))
  
  dt <- data.table(excludedInputs = excludedInputs, id = ids)
  dt <- dt[, .(number = length(list(id))), by = excludedInputs]
  
  exludeVector <- c(0)
  

  model <- trainNeuralNetwork(getTrainSet(id), hiddenLayers = FALSE, excludeVector = exludeVector)
  
  model$inputNodePos <- (1 : vars$options$windowSize)
  model$numOfExclusionPerNode <- numOfExclusionPerNode
  
  model$ids <- ids
  model$excludedInputs = dt$excludedInputs
  
  return(model)
}






findDifferenceInNeuralNetworksWrtHiddenLayers <- function() 
{
  tolerance <- 1e-5
  
  idsNNsDiffer <- unlist(lapply(names(vars$timeSeries), function(id) {
    reducedWeightsNN <- getReducedNeuralNetworkWeights(getModel('nnfe', id))[[1]][[1]][,1]
    reducedWeightsNNH <- getReducedNeuralNetworkWeights(getModel('nnfeh', id))[[1]][[1]][,1]
    
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
  neuralNetwork.tempHiddenNodes <<- vars$options$hiddenLayers
  if (is.null(neuralNetwork.hlOptimizationNN[[id]]))
  {
    #get neural network and error without hidden layer
    neuralNetwork.hlOptimization <<- c(0)
    neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
    neuralNetwork.testResults.hlOptimizationNN.old[[id]] <<- neuralNetwork.testResults.hlOptimizationNN[[id]]   
    
    expected_values <- data.expectedTestResults[[id]]

    last_error <- sMAPE(expected_values, neuralNetwork.testResults.hlOptimizationNN[[id]])
    neuralNetwork.hlOptimizationErrorVector[1] <<- last_error 
    
    #add first layer incrementally
    for (i in 1:vars$options$windowSize){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(i)
      vars$options$hiddenLayers <<- c(i)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- sMAPE(expected_values, neuralNetwork.testResults.hlOptimizationNN[[id]])
      neuralNetwork.hlOptimizationErrorVector[i+1] <<- current_error

      #break from optimization when error rises again
      if(last_error < current_error)
      {
        neuralNetwork.hlOptimization <<- c(i-1)
        return(neuralNetwork.hlOptimization)
      }
      #set new error to last error and save previous neural network
      last_error <- current_error
      neuralNetwork.testResults.hlOptimizationNN.old[[id]] <<- neuralNetwork.testResults.hlOptimizationNN[[id]]
      neuralNetwork.hlOptimization
    }

    m <- vars$options$windowSize
    #add first layer incrementally
    for (j in 1:m){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(m, j)
      vars$options$hiddenLayers <<- c(m, j)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- sMAPE(neuralNetwork.testResults.hlOptimizationNN[[id]], expected_values)
      neuralNetwork.hlOptimizationErrorVector[m+j+1] <<- current_error

      #break from optimization when error rises again
      if(last_error < current_error)
      {
        neuralNetwork.hlOptimization <<- c(m, j)
        vars$options$hiddenLayers <<- c(m, j)
        return(neuralNetwork.hlOptimization)
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
  if(!is.na( neuralNetwork.hlOptimization[2]))
  {
    n_seclayer <- neuralNetwork.hlOptimization[2]
  }
  else
  {
    n_seclayer <- 0
  }


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

  if(n_seclayer>0)
  {
    second_index <- n_seclayer
    for(l in 1 : second_index)
    {
      offset <- first_index + l
      eM[offset, 1] <- n_firstlayer
      eM[offset, 2] <- l
    }
  }
  
  return(data.table("first layer" = eM[,1], "second layer" = eM[,2], "SMAPE" = eM[,3]))
}
