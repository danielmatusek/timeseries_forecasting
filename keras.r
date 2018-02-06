library(keras)
library(tensorflow)
library(RSNNS)


use_condaenv("tensorflow")
k <- backend()


batch_size <- 32

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
  
  
  #model <- keras_model_sequential()
  #model %>% layer_lstm(32, activation = NULL, recurrent_activation = NULL, stateful = TRUE, return_sequences = TRUE,
  #  input_shape = c(vars$options$windowSize, 16), batch_size = batch_size)
  #model %>% layer_dense(1, activation = NULL)
  
  data <- data.matrix(vars$timeSeries[[id]][, -1])
  
  minIndexTrain <- 1
  minIndexTest <- nrow(data) - vars$options$horizon - vars$options$windowSize + 1
  minIndexVal <- floor((minIndexTest - 1) * 4 / 5)
  
  trainData <- data[1:(minIndexVal-1),]
  mean <- mean(trainData)
  std <- sd(trainData)
  max <- max(trainData)
  min <- min(trainData)
  #mean <- min
  #std <- max - min
  data <- scale(data, center = mean, scale = std)
  
  train_gen <- generator(data,
    lookback = vars$options$windowSize,
    minIndex = minIndexTrain,
    maxIndex = minIndexVal - 1,
    shuffle = TRUE,
    batchSize = batch_size)
  val_gen <- generator(data,
    lookback = vars$options$windowSize,
    minIndex = minIndexVal,
    maxIndex = minIndexTest - 1,
    batchSize = batch_size)
  
  val_steps = (minIndexTest - minIndexVal - 1 - vars$options$windowSize) / batch_size
  
  model <- keras_model_sequential() %>%
    layer_flatten(input_shape = list(vars$options$windowSize, dim(data)[[-1]])) %>%
    layer_dense(units = 2) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = 'mae'
    #loss = loss_mean_absolute_percentage_error
  )
  
  history <- model %>% fit_generator(
    train_gen,
    steps_per_epoch = floor(nrow(data) / batch_size * 1.5),
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
    lookback = vars$options$windowSize,
    minIndex = nrow(data) - vars$options$horizon - vars$options$windowSize + 1,
    batchSize = vars$options$horizon)
  
  predictions <- model %>% predict_generator(function() {list(test_gen()[[1]])}, steps = 1)
  return (predictions[,1]*model$std+model$mean)
}
