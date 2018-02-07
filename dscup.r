maxWindowSize <<- 2
maxHiddenLayer <<- 2
maxHiddenNodes <<- 2

startRoutine <- function(modelName, id, granularity){
    #write.csv()
    loadedID <- names(vars$timeSeries)
    if(granularity == 'monthly'){
        x <- data.frame("MODEL", "WINDOWSIZE", "LAYER ARCHITECTURE", "SMAPE")
        write.table(x, file = "monthly.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestMonthlyConfig(modelName, id)
    }
    else if (granularity == 'weekly'){
        #todo
    }
    else if (granularity == 'daily'){
        #todo
    } else print('WARN: no such granularity (dscup.r)')
}

getBestMonthlyConfig <- function(modelName, id){
    vars$options$horizon <<- 3

    #iteriere durch alle Windows und Layer
    for(windowSize in 1:maxWindowSize){
        vars$options$windowSize <<- windowSize
        for(hiddenLayer in 1:maxHiddenLayer){
            layerVector <- rep(1,hiddenLayer)
            #j <- hiddenLayer
            for(hiddenNodes in 1:((maxHiddenNodes ^ hiddenLayer)-1)){
                #Aufbauen des HIdden Nodes Vektors
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

                # Die eigentliche Verarbeitungsroutine in welcher  das Model gelernt wird 
                # (in getTestResults und dann wird der SMAPE in eine Datei rausgeschrieben)
                vars$options$hiddenLayers <<- layerVector
                testResults <- getTestResults(modelName, id)
                if(inherits(testResults, 'TestResults'))
                {
                    smape <- sMAPE(testResults$expected, testResults$predicted)  
                    x <- data.frame(modelName, windowSize, toString(layerVector), smape)
                    write.table(x, file = "monthly.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
                }
                resetModels(modelName)
            }
        }
    }                
}

getBestWeeklyConfig <- function(x){
    #todo dasselbe wie beim monat nur mit anderm horizon
    #write.csv(getBestMonthlyConfig(modelName, id), file = "weekly.csv", append = TRUE, eol = '\n')
}

getBestDailyConfig <- function(x){
    #todo dasselbe wie beim monat nur mit anderm horizon
    #write.csv(getBestMonthlyConfig(modelName, id), file = "daily.csv", append = TRUE, eol = '\n')
}