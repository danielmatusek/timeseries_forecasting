data.names <- NULL
data.sets <- NULL
data.windows <- NULL
data.trainSets <- NULL
data.testSets <- NULL
data.inputDifference.testSets <- NULL

data.windowSize <- NULL
data.horizon <- NULL
data.idSelected <- NULL

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

getTimeSeriesValueSpan <- function(id)
{
  values <- data.sets[[id]]$y
  max(values) - min(values)
}

resetWindows <- function() {
  data.trainSets <<- NULL
  data.testSets <<- NULL
}

createWindows <- function(id) {
  
  dataSet <- data.sets[[id]]$y
  
  windows <- NULL
  if(neuralNetwork.inputDifference)
  {
    
    win <- as.data.table(rollapply(dataSet, width = data.windowSize + 1, FUN = identity, by = 1), by.column = TRUE)
    names(win) <- paste0('xt', data.windowSize : 0)
    setcolorder(win, paste0('xt', 0 : data.windowSize))
  
    winDt <- as.data.table(rollapply(diff(dataSet), width = data.windowSize - 1, FUN = identity, by = 1), by.column = TRUE)
    names(winDt) <- paste0('dt', (data.windowSize - 1) : 1)
    setcolorder(winDt, paste0('dt', 1 : (data.windowSize - 1)))
    winDt <- winDt[-nrow(winDt),]
    
    win <- cbind(win, winDt)
    
    index <- 1:(nrow(win) - (data.horizon)) 
    data.trainSets[[id]] <<- win[index, ]
    data.testSets[[id]] <<- win[-index, ]
    return()
  }
  
  windows <- as.data.table(rollapply(dataSet, width = data.windowSize + 1, FUN = identity, by = 1), by.column = TRUE)
  names(windows) <- paste0('xt', data.windowSize:0)
  setcolorder(windows, paste0('xt', 0:data.windowSize))
  
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
