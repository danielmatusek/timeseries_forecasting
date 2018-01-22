library(keras)
library(tensorflow)
library(RSNNS)


#--- global
batch_size <- 1
maxlen <- vars$options$windowSize
validationset.ratio <- 0.1

#--- model
tsteps <- vars$options$windowSize
lstm.layers <- c(vars$options$windowSize,2,1)

#--- fit
epochs <- 15

#--- predict



use_condaenv("tensorflow")
k <- backend()

trainLSTM <- function(id)
{
  trainset <- getTrainSet(id)
  
  traininput <- trainset[,2:length(trainset)]
  traintarget <- trainset[,1]
  splitSet <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=validationset.ratio)
  
  x_train <- splitSet$inputsTrain
  y_train <- splitSet$targetsTrain    
  x_test <- splitSet$inputsTest
  y_test <- splitSet$targetsTest
  
  x_train <- pad_sequences(x_train, maxlen = maxlen)
  x_test <- pad_sequences(x_test, maxlen = maxlen)

  x_train <- k$eval(k$expand_dims(x_train, axis = 2L))
  x_test <- k$eval(k$expand_dims(x_test, axis = 2L))
  
  features <- dim(x_train)[3]
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(units = tsteps, input_shape = c(tsteps, features), batch_size = batch_size,
               return_sequences = FALSE, stateful = TRUE) %>%
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

getTestResults.lstm <- function(model,id)
{
  testSet <- getTestSet(id)
  x_test <- testSet[,2:length(testSet)]
  #test <- testSet[,1:vars$options$windowSize]
  test <- k$eval(k$expand_dims(x_test, axis = 2L))
  predict(model, test, batch_size)[,1]
}