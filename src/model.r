###
### Prediction Models
###

#' the wrapper method for all get model functions which calls all specific
#' functions from each package for each model
#'
#' @param modelName name of the model
#' @param id ID of the time series to get the model for
#' @return return model
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
      
      vars$models[[modelName]][[id]] <<- model
      vars$cpuTimes[[modelName]][[id]] <<- times[['elapsed']]
    }
    
    return (vars$models[[modelName]][[id]])
  }
}

#' Get the test results for a given model and a given id or for all time series
#' if id = NULL
#'
#' @param modelNamen name of the model
#' @param id id of the time series 
#' @return test results
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
    

    if (mode(model) == 'list' || mode(model) == 'numeric' || mode(model) == 'function')
    {
      testResults <- do.call(paste0('getTestResults.', modelName), list(model, id))
      if (mode(testResults) == 'numeric')
      {
        vars$predictions[[modelName]][[id]] <<- testResults
        return (structure(list(expected = tail(vars$timeSeries[[id]][,1], vars$options$horizon), predicted = testResults), class = 'TestResults'))
      }
      else
      {
        vars$predictions[[modelName]][[id]] <- NA
        return (NA)
      }
    }
    else
    {
      vars$predictions[[modelName]][[id]] <<- NA
      return (NA)
    }
  }
  else
  {
    testResults <- vars$predictions[[modelName]][[id]]
    if (mode(testResults) == 'numeric')
    {
      return (structure(list(expected = tail(vars$timeSeries[[id]][,1], vars$options$horizon), predicted = testResults), class = 'TestResults'))
    }
    else
    {
      return (NA)
    }
  }
}


#' gets CPU times for models
#'
#' @param modelName name of the model
#' @param id id of a time series, default(NULL)
#' @param na.rm Defaults to TRUE
#' @return CPU times
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

#' resets a model
#'
#' @param vector modelName in a vector which should be reset
resetModels <- function(...)
{
  for (modelName in c(...))
  {
    if (modelName %in% oneForAllModels)
    {
      vars$models[[modelName]] <<- NULL
      vars$cpuTimes[[modelName]] <<- NULL
    }
    else
    {
      vars$models[[modelName]] <<- list()
      vars$cpuTimes[[modelName]] <<- list()
    }
    
    vars$predictions[[modelName]] <<- list()
  }
}
