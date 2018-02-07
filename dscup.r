maxWindowSize <<- 2
maxHiddenLayer <<- 2
maxHiddenNodes <<- 2

horizon.monthly <<- 3
horizon.weekly <<- 4
horizon.daily <<- 9

filename <<- NULL

startRoutine <- function(modelName, id, granularity){
    loadedID <- names(vars$timeSeries)

    x <- data.frame("TS-ID", "MODEL", "WINDOWSIZE", "LAYER ARCHITECTURE", "SMAPE")

    if(granularity == 'monthly'){
        filename <<- 'monthly.csv'
        write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestConfig(modelName, id, horizon.monthly)
    }
    else if (granularity == 'weekly'){
        filename <<- 'weekly.csv'
        write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestConfig(modelName, id, horizon.weekly)
    }
    else if (granularity == 'daily'){
        filename <<- 'daily.csv'
        write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
        getBestConfig(modelName, id, horizon.daily)
    } else print('USERWARN: no such granularity (dscup.r)')
}

getBestConfig <- function(modelName, id, horizon){
    vars$options$horizon <<- horizon

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
                    x <- data.frame(id, modelName, windowSize, toString(layerVector), smape)
                    write.table(x, file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
                }
                resetModels(modelName)
            }
        }
    }                
}

# getBestWeeklyConfig <- function(x){
#     vars$options$horizon <<- 4

#     #iteriere durch alle Windows und Layer
#     for(windowSize in 1:maxWindowSize){
#         vars$options$windowSize <<- windowSize
#         for(hiddenLayer in 1:maxHiddenLayer){
#             layerVector <- rep(1,hiddenLayer)
#             #j <- hiddenLayer
#             for(hiddenNodes in 1:((maxHiddenNodes ^ hiddenLayer)-1)){
#                 #Aufbauen des HIdden Nodes Vektors
#                 inc <- FALSE
#                 j <- hiddenLayer
#                 while(!inc){
#                     if (layerVector[j] == maxHiddenNodes){
#                         if(j==1) break
#                         layerVector[j] <- 1
#                         j <- j-1 
#                     } else {
#                         layerVector[j] <- (layerVector[j] + 1)
#                         inc <- TRUE
#                     }
#                     if(inc) break
#                 }   

#                 # Die eigentliche Verarbeitungsroutine in welcher  das Model gelernt wird 
#                 # (in getTestResults und dann wird der SMAPE in eine Datei rausgeschrieben)
#                 vars$options$hiddenLayers <<- layerVector
#                 testResults <- getTestResults(modelName, id)
#                 if(inherits(testResults, 'TestResults'))
#                 {
#                     smape <- sMAPE(testResults$expected, testResults$predicted)  
#                     x <- data.frame(modelName, windowSize, toString(layerVector), smape)
#                     write.table(x, file = "monthly.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
#                 }
#                 resetModels(modelName)
#             }
#         }
#     }   
# }

# getBestDailyConfig <- function(x){
#     vars$options$horizon <<- 9

#     #iteriere durch alle Windows und Layer
#     for(windowSize in 1:maxWindowSize){
#         vars$options$windowSize <<- windowSize
#         for(hiddenLayer in 1:maxHiddenLayer){
#             layerVector <- rep(1,hiddenLayer)
#             #j <- hiddenLayer
#             for(hiddenNodes in 1:((maxHiddenNodes ^ hiddenLayer)-1)){
#                 #Aufbauen des HIdden Nodes Vektors
#                 inc <- FALSE
#                 j <- hiddenLayer
#                 while(!inc){
#                     if (layerVector[j] == maxHiddenNodes){
#                         if(j==1) break
#                         layerVector[j] <- 1
#                         j <- j-1 
#                     } else {
#                         layerVector[j] <- (layerVector[j] + 1)
#                         inc <- TRUE
#                     }
#                     if(inc) break
#                 }   

#                 # Die eigentliche Verarbeitungsroutine in welcher  das Model gelernt wird 
#                 # (in getTestResults und dann wird der SMAPE in eine Datei rausgeschrieben)
#                 vars$options$hiddenLayers <<- layerVector
#                 testResults <- getTestResults(modelName, id)
#                 if(inherits(testResults, 'TestResults'))
#                 {
#                     smape <- sMAPE(testResults$expected, testResults$predicted)  
#                     x <- data.frame(modelName, windowSize, toString(layerVector), smape)
#                     write.table(x, file = "monthly.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep=";") 
#                 }
#                 resetModels(modelName)
#             }
#         }
#     } 
# }