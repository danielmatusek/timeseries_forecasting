library(keras)
library(tensorflow)
library(RSNNS)


use_condaenv("tensorflow")
k <- backend()


lookback <- 7
delay <- 0
batch_size <- 32
minIndexTrain <- 1
minIndexVal <- 1025
minIndexTest <- 1281

generator <- function(data, lookback, minIndex, maxIndex = NULL, shuffle = FALSE, delay = 0, batchSize = 128, step = 1) {
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


trainLSTM <- function(id)
{
  #batch_size = 32
  #timesteps = 8
  #data_dim = 1
  
  #browser()
  #model <- keras_model_sequential()
  #model %>% layer_lstm(32, activation = NULL, recurrent_activation = NULL, stateful = TRUE, return_sequences = TRUE,
  #  input_shape = c(vars$options$windowSize, 16), batch_size = batch_size)
  #model %>% layer_dense(1, activation = NULL)
  
  data <- data.matrix(vars$timeSeries[[id]][, -1])
  trainData <- data[1:(minIndexVal-1),]
  mean <- mean(trainData)
  std <- sd(trainData)
  data <- scale(data, center = mean, scale = std)
  print(paste0('Mean = ', mean, ', std = ', std))
  
  train_gen <- generator(data,
    lookback = lookback,
    delay = delay,
    minIndex = minIndexTrain,
    maxIndex = minIndexVal - 1,
    shuffle = TRUE,
    batchSize = batch_size)
  val_gen <- generator(data,
    lookback = lookback,
    delay = delay,
    minIndex = minIndexVal,
    maxIndex = minIndexTest - 1,
    batchSize = batch_size)
  
  val_steps = (minIndexTest - minIndexVal - 1 - lookback) / batch_size
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = 32, input_shape = list(NULL, dim(data)[[-1]]), return_sequences = TRUE,
      dropout = 0.2, recurrent_dropout = 0.5) %>%
    layer_lstm(units = 64, return_sequences = TRUE, dropout = 0.2, recurrent_dropout = 0.5) %>%
    layer_lstm(units = 128, activation = 'relu', dropout = 0.2, recurrent_dropout = 0.5) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    #loss = 'mae'
    loss = loss_mean_absolute_percentage_error
  )
  
  history <- model %>% fit_generator(
    train_gen,
    steps_per_epoch = 500,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
  )
  
  model$mean = mean
  model$std = std
  
  return(model)
}

getTestResults.lstm <- function(model, id)
{
  data <- data.matrix(vars$timeSeries[[id]][, -1])
  data <- scale(data, center = model$mean, scale = model$std)
  test_gen <- generator(data,
    lookback = lookback,
    delay = delay,
    minIndex = nrow(data) - vars$options$horizon,
    batchSize = batch_size)
  
  test_steps = (nrow(data) - minIndexTest - 1 - lookback) / batch_size
  predictions <- model %>% predict_generator(function() {list(test_gen()[[1]])}, steps = test_steps, verbose = TRUE)
  return (predictions[,1]*model$std+model$mean)
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
  epochs <- 5
  
  #--- model
  tsteps <- vars$options$windowSize
  lstm.layers <- c(vars$options$windowSize,2,1)
  
  trainset <- as.matrix(getTrainSet(id))
  
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
  
  #browser()
  
  y_train <- train[, 1]
  y_test <- validation[, 1]
  
  train <- train[, -1]
  validation <- validation[, -1]
  
  
  x_train <- pad_sequences(train, maxlen = maxlen)
  x_test <- pad_sequences(validation, maxlen = maxlen)
#browser()
  x_train <- k$eval(k$expand_dims(x_train, axis = 2L))
  x_test <- k$eval(k$expand_dims(x_test, axis = 2L))
  
  
  
  features <- dim(x_train)[3]
  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = tsteps, input_shape = c(tsteps, features), batch_size = batch_size,
               return_sequences = TRUE, stateful = FALSE) %>%
    layer_lstm(units=tsteps, return_sequences = FALSE) %>%
    layer_dense(units = 1)
  model %>% compile(loss = 'mse', optimizer = 'rmsprop', metrics = c('accuracy'))
  
#  for (i in 1:10) {
  #browser()
  #epochs=100, batch_size=1, verbose=2)
  history <- model %>% fit(x_train, y_train, batch_size = batch_size,
                  epochs = epochs, verbose = 1, shuffle = FALSE)
    #    model %>% fit(x_train, y_train, batch_size = batch_size,
    #     epochs = 30, verbose = 1, shuffle = FALSE)
    
        
#   model %>% reset_states()
#  }
  
# x_train <- k$expand_dims(x_train, axis=2L)
#  x_train <- k$eval(x_train)
  #x_train <- array_reshape(x = x_train, dim = list(347, 1, 7))
  
#  model_sample <- dim(x_train)[1]
#  model_steps <- dim(x_train)[2]
#  model_features <- dim(x_train)[3]
  
  #Model bauen   
#  model <- keras_model_sequential()
  #
  #inputs <- layer_input(shape = c(model_features))
  #outputs <-  inputs %>%
  #            layer_lstm(input_shape = c(model_sample, model_steps, model_features), units = lstm.layers[1], return_sequences = TRUE) %>%
  #            layer_dense(units = lstm.layers[3])
  
  #model <- keras_model(inputs = inputs, outputs = outputs)

#  model %>%
#    layer_input(shape = c(347)) %>%
#    layer_lstm(units = lstm.layers[1], input_shape = c(347, 1), return_sequences = TRUE) %>% #dropout = 0.2
#    #layer_lstm(units = lstm.layers[2], return_sequences = FALSE, dropout = 0.2)
#    layer_dense(units = lstm.layers[3]) 
#  #layer_activation(activation = 'softmax')
#browser()
#  model %>%
#    layer_dense(units = lstm.layers[1], input_shape = c(348,7)) %>%
#    #layer_activation('relu') %>% 
#    layer_dense(units = lstm.layers[3]) %>% 
#    layer_activation('softmax')  
  
  # Try using different optimizers and different optimizer configs
#  model %>% compile(
#    loss = 'mse',
#    optimizer = 'rmsprop',
#    metrics = c('accuracy')
#  )
#  browser()
#  cat('Train...\n')
#  model %>% fit(
#    x = x_train, 
#    y = y_train,
#    batch_size = batch_size,
#    epochs = 1,
#    verbose = 1,
#    validation_data = list(x_test, y_test)
#  )
  
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
  #test <- testSet[,1:vars$options$windowSize]
  test <- k$eval(k$expand_dims(x_test, axis = 2L))
  predict(model, test, batch_size)[,1]
  
  #todo: predict    
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
  testSet <- as.matrix(getTestSet(id))
  testtestset <- getTestSet(id)
  
 # y_train <- train[, 1]
 # y_test <- validation[, 1]
  
 # train <- train[, -1]
 # validation <- validation[, -1]
  
  #-------------------------------
  #testSet <- getTestSet(id)
  #x_test <- testSet[,2:length(testSet)]
  #test <- testSet[,1:vars$options$windowSize]
  test <- k$eval(k$expand_dims(testSet, axis = 2L))
  predict(model, test, batch_size)[,1]
}