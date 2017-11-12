data.names <- NULL
data.sets <- NULL
data.normalized <- NULL
data.normalizationInfo <- NULL
data.windows <- NULL
data.trainSets <- NULL
data.testSets <- NULL

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
    data.normalized[[id]] <<- data.table(x = data.sets[[id]]$x, y = scale(data.sets[[id]]$y,
      center = offset, scale = scale))
    data.normalizationInfo[[id]]$scale <<- scale
    data.normalizationInfo[[id]]$offset <<- offset
  }
}

createWindows <- function(windowSize, numTestData) {
  data.normalized.ids <- names(data.normalized)
  for (i in 1:length(data.normalized.ids))
  {
    id <- data.normalized.ids[i]
    
    windows <- as.data.table(rollapply(data.normalized[[id]]$y, width = windowSize+1, FUN = identity, by = 1),
      by.column = TRUE)
    names(windows) <- paste0('xt', windowSize:0)
    
    index <- 1:(nrow(windows) - numTestData)
    data.trainSets[[id]] <<- windows[index, ]
    data.testSets[[id]] <<- windows[-index, ]
  }
}
