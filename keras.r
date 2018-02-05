library(keras)
library(tensorflow)
library(RSNNS)


use_condaenv("tensorflow")
k <- backend()

batch_size <<- 3

trainLSTM <- function(id)
{
  model <- keras_model_sequential()
  for (i in 1:length(vars$options$hiddenLayers))
  {
    hiddenNeurons <- vars$options$hiddenLayers[i]
    if (i == 1)
    {
      model %>% layer_dense(units = hiddenNeurons, input_shape = c(vars$options$windowSize))
    }
    else
    {
      model %>% layer_dense(units = hiddenNeurons)
    }
    
    model %>% layer_dropout(rate = 0.1)
  }
  model %>% layer_dense(units = vars$options$horizon)
  
  summary(model)
  
  model %>% compile(loss = loss_mean_absolute_percentage_error, optimizer = optimizer_rmsprop())
  
  dataSet <- vars$timeSeries[[id]]$y
  windows <- rollapply(dataSet, width = vars$options$windowSize + vars$options$horizon, FUN = identity, by = 1)
  trainset <- windows[-NROW(windows),]
  
  train.x <- trainset[, 1:vars$options$windowSize]
  train.y <- trainset[, -(1:vars$options$windowSize)]
  
  history <- model %>% fit(train.x, train.y, batch_size = 1)
  
  return(model)
}

getTestResults.lstm <- function(model, id)
{
  dataSet <- vars$timeSeries[[id]]$y
  windows <- rollapply(dataSet, width = vars$options$windowSize + vars$options$horizon, FUN = identity, by = 1)
  testset <- windows[NROW(windows), 1:vars$options$windowSize]
  m <- matrix(c(testset), nrow = 1, ncol = vars$options$windowSize)
  predict(model, m, batch_size = 1)[1, ]
}

trainLSTM.old.2 <- function(id)
{
  model <- keras_model_sequential()
  for (i in 1:length(vars$options$hiddenLayers))
  {
    hiddenNeurons <- vars$options$hiddenLayers[i]
    if (i == 1)
    {
      model %>% layer_dense(units = hiddenNeurons, input_shape = c(vars$options$windowSize))
    }
    else
    {
      model %>% layer_dense(units = hiddenNeurons)
    }
    
    model %>% layer_dropout(rate = 0.3)
  }
  model %>% layer_dense(units = 1)
  
  summary(model)
  
  model %>% compile(loss = loss_mean_absolute_percentage_error, optimizer = optimizer_rmsprop())
  
  trainset <- as.matrix(getTrainSet(id))
  train.x <- trainset[, -1]
  train.y <- trainset[, 1]
  
  history <- model %>% fit(train.x, train.y, batch_size = 1)
  
  return(model)
}

trainLSTM.old <- function(id)
{
  maxlen <- vars$options$windowSize
  #batch_size <- vars$options$windowSize32
  maxlen <- vars$options$windowSize
  ratio <- 0.9
  
  #--- fit
  epochs <- 100
  
  #--- model
  tsteps <- vars$options$windowSize
  lstm.layers <- c(vars$options$windowSize,2,1)
  
  trainset <- as.matrix(getNormalizedTrainSet(id))
  
  #traininput <- trainset[,2:length(trainset)]
  #traintarget <- trainset[,1]
  
  # make sure ratio divides training and test into multipe of 32 rows
  numRows <- NROW(trainset)
  maxTrainIndex <- floor(ratio * numRows / batch_size) * batch_size
  maxValidationIndex <- floor((numRows - maxTrainIndex) / batch_size) * batch_size
  if (maxValidationIndex == 0)
  {
    maxValidationIndex <- batch_size
    maxTrainIndex <- maxTrainIndex - batch_size
  }
  maxValidationIndex <- maxValidationIndex + maxTrainIndex
  #splitSet <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=ratio)
  #x_train <- splitSet$inputsTrain
  #y_train <- splitSet$targetsTrain
  #x_test <- splitSet$inputsTest
  #y_test <- splitSet$targetsTest
  
  train <- trainset[1:maxTrainIndex, ]
  
  validation <- trainset[(maxTrainIndex+1):maxValidationIndex, ]
 
  y_train <- train[, 1]
  y_test <- validation[, 1]
  
  train <- train[, -1]
  validation <- validation[, -1]
  
  
  x_train <- pad_sequences(train, maxlen = maxlen)
  x_test <- pad_sequences(validation, maxlen = maxlen)

  x_train <- k$eval(k$expand_dims(x_train, axis = 2L))
  x_test <- k$eval(k$expand_dims(x_test, axis = 2L))
  
  
  
  features <- dim(x_train)[3]
  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = tsteps, input_shape = c(tsteps, features), batch_size = batch_size,
               return_sequences = FALSE, stateful = TRUE, dropout = 0.1, unit_forget_bias = FALSE) %>%
   # layer_lstm(units=tsteps, return_sequences = FALSE, stateful = FALSE) %>%
    layer_dense(units = 1, activation = 'linear')
  model %>% compile(loss = 'mse', optimizer = 'rmsprop', metrics = c('accuracy'))
  

  history <- model %>% fit(x_train, y_train, batch_size = batch_size,
                  epochs = epochs, verbose = 1, shuffle = FALSE)
  
  scores <- model %>% evaluate(
    x_test, y_test,
    batch_size = batch_size
  )
  
  cat('Test score:', scores[[1]])
  cat('Test accuracy', scores[[2]])
  
  return(model)
}

testLSTM <- function(id)
{
  model <- trainLSTM(id)
  
  testSet <- getTestSet(id)
  x_test <- testSet[,2:length(testSet)]
  
  test <- k$eval(k$expand_dims(x_test, axis = 2L))
  predict(model, test, batch_size)[,1]
   
}


getModel.lstm <- function(id)
{
  trainLSTM(id)
}

getTestResults.lstm.old2 <- function(model, id)
{
  testSet <- as.matrix(getTestSet(id))
  predict(model, testSet, batch_size = 1)[, 1]
}

getTestResults.lstm.old <- function(model,id)
{
  testSet <- as.matrix(getNormalizedTestSet(id))
  
  test <- k$eval(k$expand_dims(testSet, axis = 2L))
  
  predicted <- predict(model, test, batch_size)[,1]
  predicted <- denormalizeData(predicted, normalizationParam)
  predicted[,1]
}