library(keras)
library(tensorflow)
library(RSNNS)

# just some code to get Keras running
use_condaenv("tensorflow")
k <- backend()

# batch size which is processed in the lstm network in an iteration
batch_size <- 32

# a generator function to prepare the data to fit the Keras models and its demanded input_shapes
# --data: the input data
# --lookback: How many observations to go back (Window Size)
# --minIndex: Indices in the data array that delimit which timesteps to draw from
#             This is useful for keeping a segment of the data for validation and another for testing
# --maxIndex: Indices in the data array that delimit which timesteps to draw from
#             This is useful for keeping a segment of the data for validation and another for testing
# --shuffle: Whether to shuffle the samples or draw them in chronological order
# --delay: how many timesteps in the future the target should be 
# --batchSize: The number of samples per batch
# --step: The period (in timesteps) in which data is sampled. 1 -> 1 timestep is a sample
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

# get the model for the LSTM network of the Keras package
# --id: id of the time series
getModel.lstm <- function(id)
{
  #batch_size = 32
  #timesteps = 8
  #data_dim = 1
  
  
  #model <- keras_model_sequential()
  #model %>% layer_lstm(32, activation = NULL, recurrent_activation = NULL, stateful = TRUE, return_sequences = TRUE,
  #  input_shape = c(vars$options$windowSize, 16), batch_size = batch_size)
  #model %>% layer_dense(1, activation = NULL)
  
  data <- vars$timeSeries[[id]]
  
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

# get the test results for a LSTM network
# --model: the model object
# --id: id of the time series
getTestResults.lstm <- function(model, id)
{
  data <- vars$timeSeries[[id]]
  data <- scale(data, center = model$mean, scale = model$std)
  test_gen <- generator(data,
    lookback = vars$options$windowSize,
    minIndex = nrow(data) - vars$options$horizon - vars$options$windowSize + 1,
    batchSize = vars$options$horizon)
  
  predictions <- model %>% predict_generator(function() {list(test_gen()[[1]])}, steps = 1)
  return (predictions[,1]*model$std+model$mean)
}
