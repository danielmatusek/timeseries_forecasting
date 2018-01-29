library(RSNNS)


trainJordan <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getTrainSet(id)
  traininput <- trainset[, 2:length(trainset)]
  traintarget <- trainset[, 1]
  
  #myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=validationset.ratio)
  
  #myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=0)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)
  
  #jordan <- RSNNS::jordan(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers,
  #  inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
  #  maxit = 500, learnFunc = "JE_Rprop")
  #rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = vars$options$hiddenLayers,  maxit = 500, linOut = FALSE
  #jordan <- RSNNS::jordan(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers#, learnFuncParams = c(0.001), 
    #maxit = 500, learnFunc = "JE_Rprop")
  jordan <- RSNNS::jordan(x = traininput, y = traintarget, size = vars$options$hiddenLayers, learnFuncParams=c(0.05),
    learnFunc = "JE_Rprop",
    linOut = TRUE, maxit = 50, hiddenActFunc = "Act_Identity")
  return(jordan)
}

getModel.jordan <- function(id)
{
  trainJordan(id, vars$options$hiddenLayers)
}

getTestResults.jordan <- function(model, id)
{
  predict(model, getTestSet(id))[,1]
}
