library(neuralnet)

#------- save greedy Models
neuralnetwork.excludedPathAsIndices   <<- NULL
neuralNetwork.excludedInputNodes      <<- list()
neuralNetwork.excludedPastModels      <<- list()
neuralNetwork.excludedPastErrors      <<- vector()
neuralNetwork.excludedInternalErrors  <<- vector()
neuralnetwork.strategies              <<- c("Greedy") 
#---------------------------

neuralnetwork.greedyErrorType     <<- 'Outsample'

neuralNetwork.hlOptimization <<- c(0)
neuralNetwork.hlOptimizationErrorVector <<- c(0)
neuralnetwork.tempHiddenNodes <<- c(0)

resetNeuralNetworks.hidden <- function()
{
  resetModels('nnfeh')
  resetModels('nnfah')
}

resetNeuralNetworks <- function()
{
  resetNeuralNetworks.hidden()
  resetNeuralNetworks.hlOptimizationNN()
  
  resetModels('nnfe')
  resetModels('nnfa')
  resetModels('nnfeeic')
  resetModels('nnfeheic')
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
    
    # remove the larger "unnecessary" fields. They fill up RAM.
    nn$response <- NULL
    nn$coveriate <- NULL
    nn$data <- NULL
    nn$covariate <- NULL
    nn$net.result <- NULL
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

getModel.nnfehei <- function(id)
{
  getExcludedInputNeuralNetwork(id, vars$options$hiddenLayers)
}



# -----Differentiable Input Models
getModel.nnfed <- function(id)
{
  trainNeuralNetwork(getDiffTrainSet(id),FALSE, NULL, TRUE)
}

getModel.nnfehd <- function(id)
{
  trainNeuralNetwork(getDiffTrainSet(id), vars$options$hiddenLayers,NULL, TRUE)
}

# ---- Excluded Input Statistics
getModel.nnfeeic <- function()
{
  getStatisticOfExcludedInputs()
}

getModel.nnfeheic <- function()
{
  getStatisticOfExcludedInputs(vars$options$hiddenLayers)
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
  testData <- if(isDiffInput){getDiffTestSet(testSetID)}else{getTestSet(testSetID)} 
  expected <- testData$xt0
  testData$xt0 <- NULL

  n <- compute(neuralNetwork, testData)
  predicted <- n$net.result[,1]
  
  structure(list(expected = expected, predicted = predicted), class = 'TestResults')
}

getTestResults.nnfe <- function(id)
{
  testNeuralNetwork(getModel('nnfe', id), id)
}

getTestResults.nnfeh <- function(id)
{
  testNeuralNetwork(getModel('nnfeh', id), id)
}

getTestResults.nnfa <- function(id)
{
  testNeuralNetwork(getModel('nnfa', id), id)
}

getTestResults.nnfah <- function(id)
{
  testNeuralNetwork(getModel('nnfah', id), id)
}

# test excluded Input
getTestResults.nnfeei <- function(id)
{
  testNeuralNetwork(getModel('nnfeei', id)$model, id)
}

getTestResults.nnfehei <- function(id)
{
  testNeuralNetwork(getModel('nnfehei', id)$model, id)
}
# test differntable Input
getTestResults.nnfed <- function(id)
{
  testNeuralNetwork(getModel('nnfed', id), id, TRUE)
}

getTestResults.nnfehd <- function(id)
{
  testNeuralNetwork(getModel('nnfehd', id), id, TRUE)
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

resetEIModels <- function()
{
  neuralnetwork.excludedPathAsIndices   <<- NULL
  neuralNetwork.excludedInputNodes      <<- list()
  neuralNetwork.excludedPastModels      <<- list()
  neuralNetwork.excludedPastErrors      <<- vector()
  neuralNetwork.excludedInternalErrors  <<- vector()
}

# calculate the best neural network model with greedy algorithm
getExcludedInputNeuralNetwork <- function(id, hiddenLayers = FALSE)
{
  
  resetEIModels()
    #calculate the first model
    model <- if (hiddenLayers) { if(is.null(id)){ getModel('nnfah')} else {getModel('nnfeh', id)}} else { if(is.null(id)) {getModel('nnfa')} else {getModel('nnfe', id)}}
    saveExcludedInputModel(model, id)
    
    path = c(0)
    excludedPathAsIndices <- c(1)
    from <- 0
    to <- 1
    
    for(i in 1 : vars$options$windowSize)
    {
      createModels(path, id, hiddenLayers)
      
      if(length(path) != vars$options$windowSize) excludedPathAsIndices <- c(excludedPathAsIndices, rep(0, vars$options$windowSize))

      from <- to + 1
      to <- to + vars$options$windowSize
     
      # check, weather the old best model is better than all new calculated models
      oldModel <- compareOldModel(path, from, to) 
      
      if(!is.null(oldModel))
      {
        if(length(path) == 1)
        {
          excludedPathAsIndices[1] <- 2
        }
        else
        {
          excludedPathAsIndices[from - vars$options$windowSize  + (path[length(path)] - 1)] <- 2
        }
        
        neuralNetwork.excludedInputNodes[[1]] <<- 'ø'
        stats <- structure(list(nodes = neuralNetwork.excludedInputNodes, smape = neuralNetwork.excludedPastErrors, internalE = neuralNetwork.excludedInternalErrors, info = excludedPathAsIndices, model = oldModel), class = 'TestExclusion')

        return(stats)
      }
      
      # search for the next best input
      idx <- NULL
      if(neuralnetwork.greedyErrorType == "Outsample")
      {
        idx <- which.min(neuralNetwork.excludedPastErrors[from : to])
      }
      else if(neuralnetwork.greedyErrorType == "Insample")
      {
        idx <- which.min(neuralNetwork.excludedInternalErrors[from : to])
      }
      path <- c(path, idx)
      excludedPathAsIndices[from + idx - 1] <- 1
    }
  
}




# it compares the old model with the models in range
# return NULL if old model is better
compareOldModel <- function(path, from, to)
{
  errors <- NULL
  if(neuralnetwork.greedyErrorType == "Outsample")
  {
    errors <- neuralNetwork.excludedPastErrors
  }
  else
  {
    errors <- neuralNetwork.excludedInternalErrors
  }
    
  pos <- 0
  if(length(path) == 1)
  {
    pos <- 1
  }
  else
  {
    pos <- from - vars$options$windowSize  + (path[length(path)] - 1)
  }
  
  if((length(path)) == (vars$options$windowSize)) return(neuralNetwork.excludedPastModels[[pos]])
  
  for(i in 1 : vars$options$windowSize)
  {
    if(!is.na(errors[[from + (i-1)]]))
    {
      if(errors[[pos]] >= errors[[from + (i-1)]])
       {
         return(NULL)
       }
    }
  }
  return(neuralNetwork.excludedPastModels[[pos]])
}

# create a model that exlude the given path and in every round it exludes an non-excluded Input
# it saves the exluded Vector, model, internal error, crossvalidation and 
createModels <- function(path, id, hiddenLayers)
{
  if((length(path)) == vars$options$windowSize) return(NULL)
  
  path = setdiff(path, 0) # don't consider 0 -> it's the bias
  for(i in 1 : vars$options$windowSize)
  {
    if(!(i %in% path))
    {
      saveExcludedInputModel(trainNeuralNetwork(getTrainSet(id), hiddenLayers, c(path, i)),id, c(path, i))
    }
    else
    {
      saveExcludedInputModel(NULL)
    }
  }
}

saveExcludedInputModel <- function(model, id = NULL, path = NULL)
{
  neuralnetwork.excludedPathAsIndices <<- c(neuralnetwork.excludedPathAsIndices, 0)
  if(is.null(model) || is.na(model))
  {
    neuralNetwork.excludedPastModels[[length(neuralNetwork.excludedPastModels) + 1]] <<- NaN
    neuralNetwork.excludedInternalErrors <<- c(neuralNetwork.excludedInternalErrors, NaN)
    neuralNetwork.excludedPastErrors <<- c(neuralNetwork.excludedPastErrors, NaN)
    neuralNetwork.excludedInputNodes[[length(neuralNetwork.excludedInputNodes) + 1]] <<- NaN
    return()
  }

  l <- length(neuralNetwork.excludedPastModels) + 1
  neuralNetwork.excludedPastModels[[l]] <<- model
  neuralNetwork.excludedInternalErrors  <<- c(neuralNetwork.excludedInternalErrors, neuralNetwork.excludedPastModels[[l]]$result.matrix[1])
  results <- testNeuralNetwork(neuralNetwork.excludedPastModels[[l]], id)
  neuralNetwork.excludedPastErrors      <<- c(neuralNetwork.excludedPastErrors, sMAPE(results$expected, results$predicted))
  neuralNetwork.excludedInputNodes[[l]] <<- path
}



getStatisticOfExcludedInputs <-function(hiddenLayers = FALSE)
{


  excludedPathCombination <- list()
  ids <- names(vars$timeSeries)
  excludedPathCounter <- c(rep(0,vars$options$windowSize))

  for(id in ids)
  {
    
    stats <- if(!hiddenLayers){getModel('nnfeei',id)}else{getModel('nnfehei',id)}
    resultIndex <- which(stats$info == 2)
    excludedPathCombination[[id]] <- stats$nodes[resultIndex]
    for(pathID in  excludedPathCombination[[id]]) excludedPathCounter[pathID] <- excludedPathCounter[pathID] + 1
    print(paste("ID: ", id, " path: ",excludedPathCombination[[id]], sep=""))
  }

  
  ids <- names(vars$timeSeries)
  excludedVectors <- unlist(lapply(ids, function(id) {
    model <- NULL
    if(!hiddenLayers)
    {
      model <- getModel('nnfeei', id)
    }
    else
    {
      model <- getModel('nnfehei', id)
    }
    
    resultIndex <- which(model$info == 2)
    paths <-  sort(model$nodes[[resultIndex]], decreasing = FALSE)
    paste(paths, collapse=",")
  }))
  
  dt <- data.table(excludedInputs = excludedVectors, id = ids)
  dt <- dt[, .(number = length(list(id))), by = excludedInputs]
  
  
  return(structure(list(ids = ids, paths = excludedPathCombination, pExcluded = (1 : vars$options$windowSize), excludedPathCounter = excludedPathCounter, pathsCombinedE = dt$excludedInputs, pathsCombinedN = dt$number),class = 'EIStatistic'))

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

    last_error <- sMAPE(neuralNetwork.testResults.hlOptimizationNN[[id]]$expected, neuralNetwork.testResults.hlOptimizationNN[[id]]$predicted)
    neuralNetwork.hlOptimizationErrorVector[1] <<- last_error 
    
    #add first layer incrementally
    for (i in 1:vars$options$windowSize){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(i)
      vars$options$hiddenLayers <<- c(i)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- sMAPE(neuralNetwork.testResults.hlOptimizationNN[[id]]$expected, neuralNetwork.testResults.hlOptimizationNN[[id]]$predicted)
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

    m <- vars$options$windowSize
    #add first layer incrementally
    for (j in 1:m){
      #Prepare environmental variables for nn calculation
      neuralNetwork.hlOptimization <<- c(m, j)
      vars$options$hiddenLayers <<- c(m, j)

      #get neural network for this id and hidden nodes vector
      neuralNetwork.testResults.hlOptimizationNN[[id]] <<- testNeuralNetwork(getNeuralNetwork(id, hlOptimization = TRUE), id)
      current_error <- sMAPE(neuralNetwork.testResults.hlOptimizationNN[[id]]$predicted, neuralNetwork.testResults.hlOptimizationNN[[id]]$expected)
      neuralNetwork.hlOptimizationErrorVector[m+j+1] <<- current_error

      #break from optimization when error rises again
      if(last_error < current_error)
      {
        neuralNetwork.hlOptimization <<- c(m, j)
        vars$options$hiddenLayers <<- c(m, j)
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
  
  return(data.table("first layer" = eM[,1], "second layer" = eM[,2], "SMAPE" = eM[,3]))
}
