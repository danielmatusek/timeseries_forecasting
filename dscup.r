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
        x <- data.frame("MODEL", "WINDOWSIZE", "LAYER ARCHITECTURE", "SMAPE")
        write.table(x, file = "weekly.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestWeeklyConfig(modelName, id)
    }
    else if (granularity == 'daily'){
        x <- data.frame("MODEL", "WINDOWSIZE", "LAYER ARCHITECTURE", "SMAPE")
        write.table(x, file = "daily.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestDailyConfig(modelName, id)
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
    vars$options$horizon <<- 4

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

getBestDailyConfig <- function(x){
    vars$options$horizon <<- 9

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