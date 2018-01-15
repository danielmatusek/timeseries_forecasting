# vars is a list of all variables worth saving in an extra file
# it will contain:
# vars$timeSeries[[id]]
# vars$enabledModels
# vars$models[[modelName]][[id]]
# vars$testResults[[modelName]][[id]]
# vars$cpuTimes[[modelName]][[id]]
vars <<- list(
  enabledModels = c('ar', 'nnfe', 'nnfeh', 'elman', 'mlp', 'mlph', 'jordan')
)

availableModels <<- c('ar', 'nnfe', 'nnfeh', 'nnfa', 'nnfah', 'elman', 'mlp', 'mlph', 'jordan', 'nnfeei', 'nnfehei', 'nnfed', 'nnfehd')
oneForAllModels <<- c('nnfa', 'nnfah')
modelColors <<- c('ar' = 'rgb(193, 5, 52)', 'nnfe' = 'rgb(0, 0, 255)', 'nnfeh' = 'rgb(0, 255, 255)',
  'nnfa' = 'rgb(255, 0, 128)', 'nnfah' = 'rgb(128, 0, 128)', 'elman' = 'rgb(255, 127, 0)', 'mlp' = 'rgb(0,96,0)',
  'mlph' ='rgb(255, 0, 0)', 'jordan' = 'rgb(0, 255, 128)', 'nnfeei' = 'rgb(20,20,20)', 'nnfehei' = 'rgb(50,50,50)', 'nnfed' = 'rgb(70,70,70)', 'nnfehd' = 'rgb(100,100,100)')

###
### Prediction Models
###

getModel <- function(modelName, id = NULL)
{
  # validate model name
  if (!modelName %in% availableModels)
  {
    stop(paste0('Model "', modelName, '" unknown'))
  }
  
  if (modelName %in% oneForAllModels)
  {
    if (is.null(vars$models[[modelName]]))
    {
      print(paste('train', modelName))
      times <- system.time(model <- do.call(paste0('getModel.', modelName), list()))
      
      if (is.null(vars$models))
      {
        vars$models <<- list()
        vars$cpuTimes <<- list()
      }
      vars$models[[modelName]] <<- model
      vars$cpuTimes[[modelName]] <<- times[['elapsed']]
    }
    
    return (vars$models[[modelName]])
  }
  else
  {
    if (is.null(vars$models[[modelName]][[id]]))
    {
      print(paste('train', modelName, 'for id', id))
      times <- system.time(model <- do.call(paste0('getModel.', modelName), list(id)))
      
      if (is.null(vars$models[[modelName]]))
      {
        vars$models[[modelName]] <<- list()
        vars$cpuTimes[[modelName]] <<- list()
      }
      vars$models[[modelName]][[id]] <<- model
      vars$cpuTimes[[modelName]][[id]] <<- times[['elapsed']]
    }
    
    return (vars$models[[modelName]][[id]])
  }
}


###
### Test Results
###

getTestResults <- function(x, ...)
{
  UseMethod('getTestResults')
}

# Get the test results for a given model and a given id or for all time series
# if id = NULL
getTestResults.default <- function(modelName, id)
{
  # validate model name
  if (!modelName %in% availableModels)
  {
    stop(paste0('Model "', modelName, '" unknown'))
  }
  
  # if id is null get the test results of all time series
  if (is.null(id))
  {
    results <- lapply(names(vars$timeSeries), function(id) { getTestResults(modelName, id) })
    
    # remove not valid test results
    results[sapply(results, function(r) { !inherits(r, 'TestResults') })] <- NULL
    
    # merge results
    exp <- unlist(lapply(results, function(r) { r$expected }))
    pred <- unlist(lapply(results, function(r) { r$predicted }))
    return (structure(list(expected = exp, predicted = pred), class = 'TestResults'))
  }
  else if (is.null(vars$testResults[[modelName]][[id]]))
  {
    # compute test results
    model <- getModel(modelName, id)
    
    if (!is.atomic(model))
    {
      vars$testResults[[modelName]][[id]] <<- do.call(paste0('getTestResults.', modelName), list(id))
    }
    else
    {
      # make sure that vars$testResults[[modelName]] is a list otherwise it will be NA
      if (is.null(vars$testResults[[modelName]]))
      {
        vars$testResults[[modelName]] <<- list()
      }
      
      vars$testResults[[modelName]][[id]] <<- NA
    }
  }
  
  return (vars$testResults[[modelName]][[id]])
}


###
### CPU Times
###

getCpuTimes <- function(modelName, id = NULL, na.rm = TRUE)
{
  if (modelName %in% oneForAllModels)
  {
    # learn the model if not already done
    if (is.null(vars$cpuTimes[[modelName]]))
    {
      getModel(modelName)
    }
    
    if (na.rm && mode(vars$models[[modelName]]) == 'logical')
    {
      return (NA)
    }
    else
    {
      return (vars$cpuTimes[[modelName]])
    }
  }
  else
  {
    if (is.null(id))
    {
      cpuTimes <- unlist(lapply(names(vars$timeSeries), function(id) {
        getCpuTimes(modelName, id)
      }))
      
      return (cpuTimes)
    }
    else
    {
      # learn the model if not already done
      if (is.null(vars$cpuTimes[[modelName]][[id]]))
      {
        getModel(modelName, id)
      }
      
      if (na.rm && mode(vars$models[[modelName]][[id]]) == 'logical')
      {
        return (NA)
      }
      else
      {
        return (vars$cpuTimes[[modelName]][[id]])
      }
    }
  }
}


###
### Resetting
###

resetModels <- function(modelName)
{
  vars$models[[modelName]] <<- NULL
  vars$testResults[[modelName]] <<- NULL
  vars$cpuTimes[[modelName]] <<- NULL
}
