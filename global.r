library(RSNNS)
# vars is a list of all variables worth saving in an extra file
# it will contain:
# vars$timeSeries[[id]]
# vars$enabledModels
# vars$models[[modelName]][[id]]
# vars$testResults[[modelName]][[id]]
# vars$cpuTimes[[modelName]][[id]]
vars <<- list(

  #enabledModels = c('ar', 'nnfe', 'nnfeh', 'elman', 'mlp', 'mlph', 'jordan'),
  enabledModels = c('jordan', 'elman', 'ar', 'mlp', 'mlph'),
  
  options = list(
    windowSize = 7,
    horizon = 7,
    arModelName = 'ar',
    excludeBias = TRUE,
    hiddenLayers = c(0)
  )
)

availableModels <<- c('ar', 'nnfe', 'nnfeh', 'nnfa', 'nnfah', 'elman', 'mlp', 'mlph', 'jordan',
  'nnfeei','mlpei', 'nnfed', 'nnfamei', 'lstm')
oneForAllModels <<- c('nnfa', 'nnfah', 'nnfamei')
modelColors <<- c('ar' = 'rgb(193, 5, 52)', 'nnfe' = 'rgb(0, 0, 255)', 'nnfeh' = 'rgb(0, 255, 255)',
  'nnfa' = 'rgb(255, 0, 128)', 'nnfah' = 'rgb(128, 0, 128)', 'elman' = 'rgb(255, 127, 0)', 'mlp' = 'rgb(0,96,0)',
  'mlph' ='rgb(255, 0, 0)', 'jordan' = 'rgb(0, 255, 128)', 'nnfeei' = 'rgb(20,200,20)','mlpei' = 'rgb(200,10,60)',
  'nnfed' = 'rgb(30,230,10)', 'nnfamei'= 'rgb(20,80,240)', 'lstm' = 'rgb(50, 123, 243)')

modelText <<- c('AR', 'neural network for one', 'neural network for one with hidden', 'neural network for all', 'neural network for all with hidden', 'Elman', 'multilayer perceptron', 'multilayer perceptron with hidden', 'Jordan',
                'neural network excluded input','MLP Excluded Input', 'NN with Difference', 'nnfamei', 'LSTM')

data.names <- NULL
data.windows <- NULL
data.trainSets <- NULL
data.testSets <- NULL
data.expectedTestResults <- NULL
data.diff.trainSets <- NULL
data.diff.testSets <- NULL
data.inputDifference.testSets <- NULL
data.normalized.trainSets <- NULL
data.normalized.testSets <- NULL
data.normalized.expectedTestResults <- NULL

normalizationParam <<- NULL


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
    vars$timeSeries <<- data
  }
  else
  {
    data <- data.table(id = data$id, x = data$x, y = data$y)
    
    # split data into sets with the same id
    data <- split(data, by = 'id')
    
    for (id in names(data))
    {
      data[[id]]$id <- NULL
    }
    
    vars$timeSeries <<- data
  }
}

getAvailableDataSources <- function()
{
  unlist(lapply(list.files('../resources'), function(filename) {
    length <- nchar(filename)
    dotPos <- length - 5
    
    if (tolower(substr(filename, dotPos, length)) == '.rdata')
    {
      return (filename)
    }
    
    dotPos <- length - 3
    if (tolower(substr(filename, dotPos, length)) == '.csv')
    {
      return (filename)
    }
    
    return (NULL)
  }))
}

saveResults <- function(name)
{
  # create dir if not already present
  # dir.create will not crash if it already exists
  dir.create(file.path('..', 'results'), showWarnings = FALSE)
  
  path = paste0('../results/', name, '.rdata')
  saveRDS(vars, path)
  print(paste0("saved results to 'results/", name, ".rdata'"))
}

getAvailableResuls <- function()
{
  unlist(lapply(list.files('../results'), function(filename) {
    length <- nchar(filename)
    dotPos <- length - 5
    
    if (substr(filename, dotPos, length) == '.rdata')
    {
      substr(filename, 1, dotPos - 1)
    }
  }))
}

loadResults <- function(name)
{
  path <- paste0('../results/', name, '.rdata')
  if (file.access(path, 4) == 0)
  {
    vars <<- readRDS(path)
    for (id in names(vars$timeSeries))
    {
      createWindows(id)
    }
    
    print(paste0("loaded results from 'results/", name, ".rdata'"))
  }
  else
  {
    warning(paste0("Cannot read file 'results/", name, ".rdata'"))
  }
}

getTimeSeriesValueSpan <- function(id)
{
  values <- vars$timeSeries[[id]]$y
  max(values) - min(values)
}
