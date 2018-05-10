isForecast <- FALSE

#' a generator function to prepare the data to fit the Keras models and its demanded input_shapes
#'
#' @param data the input data
#' @param lookback How many observations to go back (Window Size)
#' @param minIndex Indices in the data array that delimit which timesteps to draw from.
#' @param maxIndex Indices in the data array that delimit which timesteps to draw from
#' @param shuffle Whether to shuffle the samples or draw them in chronological order
#' @param delay how many timesteps in the future the target should be 
#' @param batchSize The number of samples per batch
#' @param step The period (in timesteps) in which data is sampled. 1 -> 1 timestep is a sample
#' @return list of samples and targets
windowGenerator2 <- function(data, lookback, minIndex, maxIndex = NULL, shuffle = FALSE, delay = 0, step = 1, batchSize = 128) {
  if (is.null(maxIndex))
  {
    maxIndex <- nrow(data) - delay
  }
  
  i <- minIndex + lookback
  
  function() {
    if (shuffle)
    {
      rows <- sample(c((minIndex+lookback):maxIndex), size = batchSize)
    }
    else
    {
      if (i + batchSize > maxIndex)
      {
        i <<- minIndex + lookback
      }
      rows <- c(i:min(i+batchSize-1, maxIndex))
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows), lookback / step, dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows))
    {
      indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay, 1]
    }
    
    list(samples, targets)
  }
}

#' resets the windows for the diff.trainset and diff.testset
resetWindows <- function() {
  data.diff.trainSets <<- NULL
  data.diff.testSets <<- NULL
}

#' Generate Windows with train and test sets to train, test and make forcasts
#'
#' @param id id of the time series
#' @param minIndex Indices in the data array that delimit which timesteps to draw from.
#' @param maxIndex Indices in the data array that delimit which timesteps to draw from
#' @param loookbackIndices
#' @param delay how many timesteps in the future the target should be 
#' @param seasonality  apply a parameter which season will be considered, e.g. 7 for a week, defaults to NULL
#' @param normalization whether the data must be normalized, defaults to NULL
#' @param isForecast prepares data for a forecast "behind" the existing dataset
#' @return returns windows
generateWindows <- function(id, minIndex = 1, maxIndex = NULL,
  lookbackIndices = NULL, delay = 0, seasonality = NULL, normalization = NULL,
  isForecast = FALSE)
{
  if (mode(lookbackIndices) != 'numeric')
  {
    lookbackIndices <- (1:vars$options$windowSize) + delay
    if (mode(seasonality) == 'numeric' && length(seasonality))
    {
      seasonIndex <- (floor(delay / seasonality) + 1) * seasonality
      if (!seasonIndex %in% lookbackIndices)
      {
        lookbackIndices <- c(lookbackIndices, seasonIndex)
      }
    }
  }
  maxLookbackIndex <- max(lookbackIndices)
  
  data <- vars$timeSeries[[id]]
  if (!is.null(normalization))
  {
    data <- normalizeData(data, normalization)
  }
  
  if (is.null(maxIndex) || maxIndex > nrow(data))
  {
    maxIndex <- nrow(data)
  }
  if (maxIndex <= 0)
  {
    maxIndex <- nrow(data) + maxIndex
  }
  if (minIndex <= 0)
  {
    minIndex <- nrow(data) + minIndex
  }
  if (minIndex - maxLookbackIndex < 1)
  {
    minIndex <- maxLookbackIndex + 1
  }
  if (maxIndex - minIndex < 0)
  {
    return(NULL)
  }
  
  if (isForecast)
  {
    samples <- array(0, dim = c(maxIndex - minIndex + 1, length(lookbackIndices)))
    colnames(samples) <- paste0('xt', lookbackIndices)
  }
  else
  {
    samples <- array(0, dim = c(maxIndex - minIndex + 1, length(lookbackIndices) + 1))
    colnames(samples) <- paste0('xt', c(0, lookbackIndices))
  }
  current <- minIndex
  if (isForecast)
  {
    current <- current + vars$options$horizon
  }
  for (i in 1:nrow(samples))
  {
    if (isForecast)
    {
      indices <- current - lookbackIndices
    }
    else
    {
      indices <- c(current, current - lookbackIndices)
    }
    samples[i,] <- data[indices,]
    current <- current + 1
  }
  
  if (!is.null(normalization))
  {
    attr(samples, 'normParams') <- getNormParameters(data)
  }
  
  return (samples)
}

#' create the windows with additional differences of the input nodes 
#'
#' @param id time series id 
#' @return returns the trainset and testset with differences of the input nodes 
createDifferentableWindow <- function(id)
{
  dataSet <- vars$timeSeries[[id]][,1]
  win <- as.data.table(rollapply(dataSet, width = vars$options$windowSize + 1, FUN = identity, by = 1), by.column = TRUE)
  names(win) <- paste0('xt', vars$options$windowSize : 0)
  setcolorder(win, paste0('xt', 0 : vars$options$windowSize))
  
  winDt <- as.data.table(rollapply(diff(dataSet), width = vars$options$windowSize - 1, FUN = identity, by = 1), by.column = TRUE)
  names(winDt) <- paste0('dt', (vars$options$windowSize - 1) : 1)
  setcolorder(winDt, paste0('dt', 1 : (vars$options$windowSize - 1)))
  winDt <- winDt[-nrow(winDt),] 
  
  win <- cbind(win, winDt)
  
  index <- 1:(nrow(win) - (vars$options$horizon)) 
  data.diff.trainSets[[id]] <<- win[index, ]
  data.diff.testSets[[id]] <<- win[-index, ]
}

#' get the train set for a time series id 
#'
#' @param id id of the time series 
#' @param delay 
#' @param seasonality apply a parameter which season will be considered, e.g. 7 for a week
#' @param normalization normalization of data, Defaults to NULL
#' @return trainset 
getTrainSet <- function(id, delay = vars$options$horizon - 1, seasonality = vars$options$seasonality, normalization = NULL)
{
  return(generateWindows(id, maxIndex = -vars$options$horizon, delay = delay,
    seasonality = seasonality, normalization = normalization))
}

#' gets all trainsets 
getAllTrainSetsCombined <- function()
{
  return(do.call(rbind, lapply(names(vars$timeSeries), function(id) { getTrainSet(id) })))
}

#' get the train set with the differences between the input nodes 
#'
#' @param id 
#' @return train set with differences
getDiffTrainSet <- function(id)
{
  if(is.null(data.diff.trainSets[[id]]))
  {
    createDifferentableWindow(id)
  }
  return(data.diff.trainSets[[id]])
}

#' get the test set for a time series id 
#'
#' @param id id of the time series 
#' @param delay lets the window start with an offset(delay) before earlier than now
#' @param seasonality apply a parameter which season will be considered, e.g. 7 for a week
#' @param normalization normalization of data, Defaults to NULL
#' @return testset 
getTestSet <- function(id, delay = vars$options$horizon - 1, seasonality = vars$options$seasonality, normalization = NULL)
{
  testSet <- generateWindows(id, minIndex = -vars$options$horizon + 1, delay = delay,
    seasonality = seasonality, normalization = normalization, isForecast = isForecast)
  if (isForecast)
  {
    return(testSet)
  }
  else
  {
    if (ncol(testSet) == 2)
    {
      return(data.matrix(testSet[, -1]))
    }
    else
    {
      return(testSet[, -1])
    }
  }
}

#' get the test set with the differences between the input nodes 
#'
#' @param id 
#' @return test set with differences
getDiffTestSet <- function(id)
{
  if(is.null(data.diff.testSets[[id]]))
  {
    createDifferentableWindow(id)
  }
  return(data.diff.testSets[[id]])
}
