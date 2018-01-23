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

# Get the test results for a given model and a given id or for all time series
# if id = NULL
getTestResults <- function(modelName, id)
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
  else if (is.null(vars$predictions[[modelName]][[id]]))
  {
    # compute test results
    model <- getModel(modelName, id)
    
    if ((mode(model) == 'list' || mode(model) == 'numeric') && !is.na(model[[1]]))
    {
      testResults <- do.call(paste0('getTestResults.', modelName), list(model, id))
      if (mode(testResults) == 'numeric')
      {
        
        vars$predictions[[modelName]][[id]] <<- testResults
        return (structure(list(expected = data.expecetedTestResults[[id]], predicted = testResults), class = 'TestResults'))
      }
      else
      {
        # make sure that vars$predictions[[modelName]] is a list otherwise it will testbe NA
        if (is.null(vars$predictions[[modelName]]))
        {
          vars$predictions[[modelName]] <<- list()
        }
        
        vars$predictions[[modelName]][[id]] <- NA
        return (NA)
      }
    }
    else
    {
      # make sure that vars$predictions[[modelName]] is a list otherwise it will be NA
      if (is.null(vars$predictions[[modelName]]))
      {
        vars$predictions[[modelName]] <<- list()
      }
      
      vars$predictions[[modelName]][[id]] <<- NA
    }
  }
  else
  {
    testResults <- vars$predictions[[modelName]][[id]]
    if (mode(testResults) == 'numeric')
    {
      return (structure(list(expected = data.expecetedTestResults[[id]], predicted = testResults), class = 'TestResults'))
    }
    else
    {
      return (NA)
    }
  }
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
      cpuTimes <- unlist(lapply(names(vars$timeSeries), function(id) { getCpuTimes(modelName, id) }))
      
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

resetModels <- function(...)
{
  for (modelName in c(...))
  {
    vars$models[[modelName]] <<- NULL
    vars$predictions[[modelName]] <<- NULL
    vars$cpuTimes[[modelName]] <<- NULL
  }
}
