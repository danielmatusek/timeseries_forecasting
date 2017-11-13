splitWindows <- function(windows, splitFactor) {
  index <- 1:nrow(windows)
  trainindex <- sample(index, trunc(length(index) * splitFactor))
  trainset <- windows[trainindex, ]
  testset <- windows[-trainindex, ]
  
  list(trainset = trainset, testset = testset)
}
