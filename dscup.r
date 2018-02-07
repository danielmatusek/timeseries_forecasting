maxWindowSize <<- 5
maxHiddenLayer <<- 3
maxHiddenNodes <<- 4

startRoutine <- function(modelName, id, granularity){
    #write.csv()
    loadedID <- names(vars$timeSeries)
    if(granularity == 'monthly'){
        getBestMonthlyConfig(modelName, id)
    }
    else if (granularity == 'weekly'){
        write.csv(getBestMonthlyConfig(modelName, id), file = "weekly.csv", append = TRUE, eol = '\n')
    }
    else if (granularity == 'daily'){
        write.csv(getBestMonthlyConfig(modelName, id), file = "daily.csv", append = TRUE, eol = '\n')
    } else print('WARN: no such granularity (dscup.r)')
}

getBestMonthlyConfig <- function(modelName, id){
    vars$options$horizon <<- 3

    for(windowSize in 1:maxWindowSize){
        vars$options$windowSize <<- windowSize
        for(hiddenLayer in 1:maxHiddenLayer){
            layerVector <- rep(1,hiddenLayer)
            #j <- hiddenLayer
            for(hiddenNodes in 1:((maxHiddenNodes ^ hiddenLayer)-1)){
                inc <- FALSE
                j <- hiddenLayer
                while(!inc){
                    if (layerVector[j] == maxHiddenNodes){
                        if(j==1) break
                        layerVector[j] <- 1
                        j <- j-1 
                    } else {
                        layerVector[j] <- (layerVector[j] + 1)
                        inc <- TRUE
                    }
                    if(inc) break
                }   
                vars$options$hiddenLayers <<- layerVector
                cat("hidden layer: ", vars$options$hiddenLayers, "\n")
                cat("windowSize: ",vars$options$windowSize, "\n")
                testResults <- getTestResults(modelName, id)
                if(inherits(testResults, 'TestResults'))
                {
                    smape <- sMAPE(testResults$expected, testResults$predicted)  
                    write.csv(smape, file = "monthly.csv", append = TRUE, eol = '\n') 
                    print(smape)
                }
                resetModels(modelName)
            }
        }
    }                
}

getBestWeeklyConfig <- function(x){

}

getBestDailyConfig <- function(x){

}