library(data.table)
library(forecast)
library(hydroGOF) #rmse function
library(neuralnet)
library(stats)
library(TSPred) #sMape function
library(zoo)

source('autoRegression.r')
source('neuralNetwork.r')

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB

getPredictionPlotly <- function(testResults, id) {
  if (is.null(testResults) || is.null(id))
  {
    return(NULL)
  }
  
  prediction <- rbindlist(list(
    as.data.table(
      rep(NA, length(data.normalized[[id]]$y) - length(testResults[[id]]$net.result))),
    as.data.table(testResults[[id]]$net.result)
  ))
  names(prediction) <- c('prediction')
  prediction$x <- data.normalized[[id]]$x
  prediction$y <- data.normalized[[id]]$y
  p <- plot_ly(prediction, x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Original') %>%
    add_trace(y = ~prediction, name = 'Prediction', line = list(dash = 'dash'))
  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
  p
}

server <- function(input, output) {

  database <- reactive({
    file <- input$dataFile

    if (!is.null(file))
    {
      #read.table(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
      data <- read.csv(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
      parseData(data, xName = input$x_axis, yName = input$y_axis)
      
      data.sets
    }
  })
  
  dataNormalized <- reactive({
    database()
    normalizationMethod <- input$normalizationRadioButton
    
    if (!is.null(data.sets) && !is.null(normalizationMethod))
    {
      normalizeData(normalizationMethod)
      
      data.normalized
    }
  })

	dataset <- reactive({
	  dataNormalized()
		id <- input$idSelect

		if (!is.null(data.normalized) && !is.null(id))
		{
		  data.normalized[[id]]
		}
	})
	
	output$x_axis <- renderUI({ 
	  df <- database()
	  if (is.null(data.names)) return()
	  selectInput("x_axis", "x-Axis", data.names$orig, selected = data.names$orig[2])
	})
	
	output$y_axis <- renderUI({
	  df <- database()
	  if (is.null(data.names)) return()
	  selectInput("y_axis","y-Axis", data.names$orig, selected = data.names$orig[3])
	})
	
	output$windowSizeSlider <- renderUI({
	  database()
	  id <- input$idSelect
	  
	  if (is.null(data.sets) || is.null(id))
	  {
	    return(NULL)
	  }
	  numData <- length(data.sets[[id]]$x)
	  sliderInput('windowSizeSlider', 'Window Size', 1, max(numData)*0.1, max(numData)*0.05, step = 1)
	})
	
	windowsCreated <- reactive({
	  dataNormalized()
	  windowSize <- input$windowSizeSlider
	  horizon <- input$horizonSlider
	  
	  if (!is.null(data.normalized))
	  {
	    createWindows(windowSize, horizon)
	    
	    list(trainSets = data.trainSets, testSets = data.testSets)
	  }
	})
	
	windowSplit <- reactive({
	  windowsCreated()
	  id <- input$idSelect
	  
	  if (!is.null(data.testSets))
	  {
	    list(trainset = data.trainSets[[id]], testset = data.testSets[[id]])
	  }
	})

	neuralNetworksTrained <- reactive({
	  windowsCreated()
	  
		if (!is.null(data.trainSets))
		{
		  trainNeuralNetworks(input$biasCheckbox)
		  
		  list(forEach = neuralNetwork.forEach, forAll = neuralNetwork.forAll)
		}
	})
	
	neuralNetworksTested <- reactive({
	  neuralNetworksTrained()
	  
	  if (!is.null(neuralNetwork.forEach))
	  {
	    testNeuralNetworks()
	    
	    list(neuralNetwork.testResults.forEach, neuralNetwork.testRestlts.forEach.hiddenLayers,
	      neuralNetwork.testResults.forAll, neuralNetwork.testRestlts.forAll.hiddenLayers)
	  }
	})

	output$idSelectBox <- renderUI({
	  database()

		if (!is.null(data.sets))
		{
		  selectInput("idSelect", "Dataset", names(data.sets))
		}
	})
	
	output$normalizationRadioButton <- renderUI({
	  db <- database()
	  
	  if (!is.null(db))
	  {
	    radioButtons('normalizationRadioButton', 'Normalization',
	      c('None' = 'none', 'Z-Normalization' = 'zScore', 'Min-Max Scale' = 'minmax'), 'none')
	  }
	})	
	
	output$horizonSlider <- renderUI({
	  db <- database()
	  
	  if (is.null(db))
	  {
	    return(NULL)
	  }
	  
	  sliderInput('horizonSlider', 'Predict Values', 1, 50, 10, step = 1)
	  
	})

	output$dataChart <- renderPlotly({
	  dataNormalized()
		id <- input$idSelect

		if (is.null(data.normalized) || is.null(id))
		{
		  return(NULL)
		}
		
		p <- plot_ly(data.normalized[[id]], x = ~x, y = ~y, type = 'scatter', mode = 'lines')
		p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
		p
	})
	
	output$dataTable <- renderDataTable(dataset())
	output$trainDataTable <- renderDataTable(windowSplit()$trainset)
	output$testDataTable <- renderDataTable(windowSplit()$testset)
  
	
	
	output$neuralNetworkChart <- renderPlot({
	  neuralNetworksTrained()
	  id <- input$idSelect

		if (!is.null(neuralNetwork.forEach) && !is.null(id))
		{
		  plot(neuralNetwork.forEach[[id]], rep = "best")
		}
	})
	
	output$neuralNetworkForecastForEachChart <- renderPlotly({
	  neuralNetworksTested()
	  getPredictionPlotly(neuralNetwork.testResults.forEach, input$idSelect)
	})
	output$neuralNetworkForecastForEachHiddenChart <- renderPlotly({
	  neuralNetworksTested()
	  getPredictionPlotly(neuralNetwork.testResults.forEach.hiddenLayers, input$idSelect)
	})
	output$neuralNetworkForecastForAllChart <- renderPlotly({
	  neuralNetworksTested()
	  getPredictionPlotly(neuralNetwork.testResults.forAll, input$idSelect)
	})
	output$neuralNetworkForecastForAllHiddenChart <- renderPlotly({
	  neuralNetworksTested()
	  getPredictionPlotly(neuralNetwork.testResults.forAll.hiddenLayers, input$idSelect)
	})
	
	
	
	
	
	
	

	aRModel <- reactive({
	    meterid <- input$idSelect
	    dataSplitFactor <- input$dataSplitSlider / 100;
	    
	    if(is.null(meterid))
	    {
	      return(NULL)
	    }
	    
	    db <- dataset()
	    
	    if(is.null(db))
	    {
	      return(NULL)
	    }
	    
	    
	    df = db$y
	    winSize = input$windowSizeSlider
	    numPredictValues = input$horizonSlider
	    browser()
	    
	    to = round(length(df) * dataSplitFactor)
	    trainData = df[1: to]
	    aRModel = arima(ts(trainData,start = 1, end = to), order= c(winSize,0,0))
	    forecast(aRModel, h = numPredictValues)
	})
	
	
	output$aRChart <- renderPlotly({
	  
	    db = dataset()
	    
  	  aRMList = getARModelList(db$consumption, input$windowSizeSlider, input$dataPrediction)
	    if (is.null(aRMList))
	    {
	      return(NULL)
	    }
  	  
  	  getPlotlyModel(aRMList, db$consumption, input$dataPrediction)
	  })

	

	output$arMLE <- renderDataTable({
	  
	  db = dataset()
	  if(is.null(db))
	  {
	    return(NULL)
	  }
	  data.frame( MSE = getMLE(db$consumption, input$windowSizeSlider, input$dataPrediction)$mse)
	})
	
	output$arCoef <- renderDataTable({
	  
	  db = dataset()
	  if(is.null(db))
	  {
	    return(NULL)
	  }

	  data.frame(coef = getCoef(db$y, input$windowSizeSlider, input$dataPrediction)$coef)
	})
	

	error_metric <- function(forecast_set, test_set, vergleich){
	  #browser()
	  rmse <- rmse(forecast_set, test_set)
	  smape <- sMAPE(test_set, forecast_set)
	  data.frame(rmse,smape, sqrt(vergleich))
	}
	
	output$ErrorMetricTable <- renderDataTable({
	  result <- neuralNetworkTest()
	  error_metric(result$net.result[,1], result$net.expected, result$net.mse)
	})

	
}
