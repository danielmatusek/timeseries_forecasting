# vars is a list of all variables worth saving in an extra file
# it will continue:
# vars$timeSeries[[id]]
# vars$enabledModels
# vars$models[[modelName]][[id]]
# vars$testResults[[modelName]][[id]]
# vars$cpuTimes[[modelName]][[id]]
vars <<- list(
  enabledModels = c('ar', 'nnfe', 'nnfeh') #, 'elman', 'mlp', 'mlph', 'jordan'
)

availableModels <<- c('ar', 'nnfe', 'nnfeh', 'nnfa', 'nnfah', 'elman', 'mlp', 'mlph', 'jordan')
oneForAllModels <<- c('nnfa', 'nnfah')

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
