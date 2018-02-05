 library(RSNNS)
 
 validationset.ratio <<- 0.05
 
 
trainRNN <- function(id, hiddenLayers = c(0))
{
  set.seed(1)
  trainset <- getNormalizedTrainSet(id)
  traininput <- trainset[, 2:length(trainset)]
  traintarget <- trainset[, 1]

  myset <- RSNNS::splitForTrainingAndTest(traininput, traintarget, ratio=validationset.ratio)
  #myset <- RSNNS::normTrainingAndTestSet(myset, type = "0_1", dontNormTargets = FALSE)

  rnn <- RSNNS::elman(x = myset$inputsTrain, y = myset$targetsTrain, size = vars$options$hiddenLayers,
                    inputsTest = myset$inputsTest, targetsTest = myset$targetsTest, learnFuncParams = c(0.001), 
                    maxit = 500, learnFunc = "JE_Rprop")
  rnn$snnsObject$setTTypeUnitsActFunc("UNIT_INPUT", "Act_Identity")
  #learnFunc = "Std_Backpropagation", , size = vars$options$hiddenLayers,  maxit = 500, linOut = FALSE
  
  return(rnn)
}

getModel.elman <- function(id)
{
  trainRNN(id, vars$options$hiddenLayers)
}

getTestResults.elman <- function(model, id)
{
  datanew <- predict(model, getNormalizedTestSet(id))[,1]
  denormalized <- denormalizeData(datanew, normalizationParam)
  denormalized[,1]
}
