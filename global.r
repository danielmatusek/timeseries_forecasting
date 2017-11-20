data.names <- NULL
data.sets <- NULL
data.normalized <- NULL
data.normalizationInfo <- NULL
data.windows <- NULL
data.trainSets <- NULL
data.testSets <- NULL

data.windowSize <- NULL
data.horizon <- NULL

parseData <- function(data, idName = NULL, xName = NULL, yName = NULL) {
  # save names
  if (length(data) >= 3)
  {
    data.names$id <<- if (is.null(idName)) names(data)[1] else idName
    data.names$x <<- if (is.null(xName)) names(data)[2] else xName
    data.names$y <<- if (is.null(yName)) names(data)[3] else yName
  }
  else
  {
    data.names$x <<- if (is.null(xName)) names(data)[1] else xName
    data.names$y <<- if (is.null(yName)) names(data)[2] else yName
  }
  data.names$orig <<- names(data)
  
  # raname the columns
  newNames <- names(data)
  for (i in 1:length(newNames))
  {
    if (newNames[i] == data.names$id)
    {
      newNames[i] <- 'id'
    }
    else if (newNames[i] == data.names$x)
    {
      newNames[i] <- 'x'
    }
    else if (newNames[i] == data.names$y)
    {
      newNames[i] <- 'y'
    }
  }
  names(data) <- newNames
  
  # subset the data frame if there are too many columns
  if (is.null(data.names$id))
  {
    data <- data.table(x = data$x, y = data$y)
    data.sets <<- data
  }
  else
  {
    data <- data.table(id = data$id, x = data$x, y = data$y)
    
    # split data into sets with the same id
    data.sets <<- split(data, by = 'id')
  }
}

normalizeData <- function(method = 'none') {
  data.sets.ids <- names(data.sets)
  for (i in 1:length(data.sets.ids))
  {
    id <- data.sets.ids[i]
    
    # calculate scale and offset
    scale <- 1
    offset <- 0
    if (method == 'zScore')
    {
      scale <- sd(data.sets[[id]]$y)
      offset <- mean(data.sets[[id]]$y)
    }
    else if (method == 'minmax')
    {
      minValue <- min(data.sets[[id]]$y)
      maxValue <- max(data.sets[[id]]$y)
      
      scale <- maxValue - minValue
      offset <- minValue
    }
    
    # scale data and save scale information
    data.normalized[[id]] <<- data.table(x = data.sets[[id]]$x,
      y = scale(data.sets[[id]]$y, center = offset, scale = scale))
    names(data.normalized[[id]]) <<- c('x', 'y')
    
    data.normalizationInfo[[id]]$scale <<- scale
    data.normalizationInfo[[id]]$offset <<- offset
  }
}

resetWindows <- function() {
  data.trainSets <<- NULL
  data.testSets <<- NULL
}

createWindows <- function(id) {
  windows <- as.data.table(rollapply(data.sets[[id]]$y, width = data.windowSize+1, FUN = identity, by = 1),
    by.column = TRUE)
  names(windows) <- paste0('xt', data.windowSize:0)
  
  index <- 1:(nrow(windows) - data.horizon)
  data.trainSets[[id]] <<- windows[index, ]
  data.testSets[[id]] <<- windows[-index, ]
}

getTrainSet <- function(id) {
  if (is.null(data.trainSets[[id]]))
  {
    createWindows(id)
  }
  
  return(data.trainSets[[id]])
}

getAllTrainSetsCombined <- function() {
  ids <- names(data.sets)
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

error_metric <- function(test_set, forecast_set){
  #forecast_set ist von Datentyp matrix, muss aber numeric sein
  forecast_set <- as.numeric(forecast_set)
  test_set <- as.numeric(test_set)
  mse <- mse(test_set, forecast_set)
  rmse <- rmse(test_set, forecast_set)
  smape <- sMAPE(test_set, forecast_set)
  data.frame(mse = mse,rmse = rmse, smape = smape)
}


