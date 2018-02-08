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

resetWindows <- function() {
  data.trainSets <<- NULL
  data.testSets <<- NULL
  data.diff.trainSets <<- NULL
  data.diff.testSets <<- NULL
  data.normalized.trainSets <<- NULL
  data.normalized.testSets <<- NULL
}


generateWindows <- function(id, lookbackIndices = NULL, delay = 0, seasonality = NULL, normalization = NULL)
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
  maxIndex <- max(lookbackIndices)
  
  data <- data.matrix(vars$timeSeries[[id]]$y)
  if (!is.null(normalization))
  {
    data <- normalizeData(data, normalization)
  }
  
  samples <- array(0, dim = c(nrow(data) - maxIndex, length(lookbackIndices) + 1))
  colnames(samples) <- paste0('xt', c(0, lookbackIndices))
  for (i in 1:nrow(samples))
  {
    current <- maxIndex + i
    indices <- c(current, current - lookbackIndices)
    samples[i,] <- data[indices,]
  }
  
  if (!is.null(normalization))
  {
    attr(samples, 'normParams') <- getNormParameters(data)
  }
  
  return (samples)
}

createWindows <- function(id)
{
  windows <- generateWindows(id)
  windows <- as.data.table(windows, by.column = TRUE)
  
  index <- 1:(nrow(windows) - vars$options$horizon)
  data.trainSets[[id]] <<- windows[index, ]
  data.testSets[[id]] <<- windows[-index, ]
  data.expectedTestResults[[id]] <<- data.testSets[[id]]$xt0
  data.testSets[[id]]$xt0 <<- NULL
}

createNormalizedWindows <- function(id)
{
  windows <- generateWindows(id, normalization = '0_1')
  normalizationParam <<- getNormParameters(windows)
  windows <- as.data.table(windows, by.column = TRUE)
  
  index <- 1:(nrow(windows) - vars$options$horizon)
  data.normalized.trainSets[[id]] <<- windows[index, ]
  data.normalized.testSets[[id]] <<- windows[-index, ]
  
  denormalizedTestSet <- denormalizeData(data.normalized.testSets[[id]], normalizationParam)
  
  data.expectedTestResults[[id]] <<- denormalizedTestSet[,1]
  data.normalized.testSets[[id]]$xt0 <<- NULL
}


createDifferentableWindow <- function(id)
{
  dataSet <- vars$timeSeries[[id]]$y
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


getTrainSet <- function(id, lookbackIndices = NULL)
{
  if (mode(lookbackIndices) != 'numeric')
  {
    lookbackIndices <- 1:vars$options$windowSize
  }
  maxIndex <- max(lookbackIndices)
  
  data <- vars$timeSeries[[id]]$y
  
  samples <- array(0, dim = c(length(data) - maxIndex, length(lookbackIndices) + 1))
  for (i in 1:nrow(samples))
  {
    current <- maxIndex + i
    indices <- c(current, current - lookbackIndices)
    samples[i,] <- data[indices]
  }
  
  windows <- as.data.table(samples, by.column = TRUE)
  names(windows) <- paste0('xt', c(0, lookbackIndices))
  
  index <- 1:(nrow(windows) - vars$options$horizon)
  return (windows[index, ])
}


getDiffTrainSet <- function(id)
{
  if(is.null(data.diff.trainSets[[id]]))
  {
    createDifferentableWindow(id)
  }
  return(data.diff.trainSets[[id]])
}

getDiffTestSet <- function(id)
{
  if(is.null(data.diff.testSets[[id]]))
  {
    createDifferentableWindow(id)
  }
  return(data.diff.testSets[[id]])
}

getAllTrainSetsCombined <- function() {
  ids <- names(vars$timeSeries)
  for (i in 1:length(ids))
  {
    getTrainSet(ids[i])
  }
  
  return(rbindlist(data.trainSets))
}

getTestSet <- function(id) {
  if (is.null(data.testSets[[id]]))
  {
    createWindows(id)
  }
  return(data.testSets[[id]])
}

getNormalizedTrainSet <- function(id) {
  if (is.null(data.normalized.trainSets[[id]]))
  {
    createNormalizedWindows(id)
  }
  return(data.normalized.trainSets[[id]])
}

getNormalizedTestSet <- function(id) {
  if (is.null(data.normalized.testSets[[id]]))
  {
    createNormalizedWindows(id)
  }
  return(data.normalized.testSets[[id]])
}
