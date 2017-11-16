library(data.table)
library(forecast)
library(hydroGOF) #rmse function
library(neuralnet)
library(stats)
library(TSPred) #sMape function
library(zoo)

source('autoRegression.r')
source('neuralNetwork.r')
source('comparision.r')

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB

getPredictionPlotly <- function(testResults, id) {
  if (is.null(testResults) || is.null(id))
  {
    return(NULL)
  }
  
  prediction <- rbindlist(list(
    as.data.table(rep(NA, length(data.sets[[id]]$y) - length(testResults[[id]]$net.result))),
    as.data.table(testResults[[id]]$net.result)
  ))
  names(prediction) <- c('prediction')
  
  prediction$x <- data.sets[[id]]$x
  prediction$y <- data.sets[[id]]$y
  
  startIndex = length(data.sets[[id]]$y) - length(testResults[[id]]$net.result)
  prediction$prediction[[startIndex]] <- prediction$y[[startIndex]]
  
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
	
	output$x_axis <- renderUI(   { 
	  df <- database()
	  if (is.null(data.names)) return()
	  selectInput("x_axis", "x-Axis", data.names$orig, selected = data.names$orig[2])
	})
	
	output$y_axis <- renderUI({
	  df <- database()
	  if (is.null(data.names)) return()
	  selectInput("y_axis","y-Axis", data.names$orig, selected = data.names$orig[3])
	})
	
	output$hiddenSliderInput <- renderUI({
	  if (is.null(input$windowSizeSlider)) return()
	  sliderInput("hiddenSliderInput","Number Hidden Neurons", 1, input$windowSizeSlider, 1,  step = 1)
	})
	
	output$windowSizeSlider <- renderUI({
	  database()
	  id <- input$idSelect
	  
	  if (is.null(data.sets) || is.null(id))
	  {
	    return(NULL)
	  }
	  numData <- length(data.sets[[id]]$x)
	  sliderInput('windowSizeSlider', 'Window Size', 1, value = 7,  max(numData)*0.1, max(numData)*0.05, step = 1)
	})
	
	windowsCreated <- eventReactive(input$ButtonClick, {
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
		  trainNeuralNetworks(input$biasCheckbox, c(input$hiddenSliderInput))
		  
		  list(forEach = neuralNetwork.forEach, forAll = neuralNetwork.forAll)
		}
	})
	
	neuralNetworksTested <- reactive({
	  neuralNetworksTrained()
	  
	  if (!is.null(neuralNetwork.forEach))
	  {
	    testNeuralNetworks()
	    
	    list(neuralNetwork.testResults.forEach, neuralNetwork.testResults.forEach.hiddenLayers,
	      neuralNetwork.testResults.forAll, neuralNetwork.testResults.forAll.hiddenLayers)
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
	      c('None' = 'none', 'Z-Normalization' = 'zScore', 'Min-Max Scale' = 'minmax'), 'minmax')
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
	
	output$neuralNetworkHiddenChart <- renderPlot({
	  neuralNetworksTrained()
	  id <- input$idSelect
	  
	  if (!is.null(neuralNetwork.forEach.hiddenLayers) && !is.null(id))
	  {
	    plot(neuralNetwork.forEach.hiddenLayers[[id]], rep = "best")
	  }
	})
	
	output$neuralNetworkChartForAll <- renderPlot({
	  neuralNetworksTrained()
	  
	  if (!is.null(neuralNetwork.forAll)) 
	  {
	    plot(neuralNetwork.forAll, rep = "best")
	  }
	})
	
	output$neuralNetworkHiddenChartForALL <- renderPlot({
	  neuralNetworksTrained()
	  
	  if (!is.null(neuralNetwork.forAll.hiddenLayers))
	  {
	    plot(neuralNetwork.forAll.hiddenLayers, rep = "best")
	  }
	})
	
	output$neuralNetworkForecastForEachChart <- renderPlotly({
	  neuralNetworksTested()
	  return (getPredictionPlotly(neuralNetwork.testResults.forEach, input$idSelect))
	})
	output$neuralNetworkForecastForEachHiddenChart <- renderPlotly({
	  neuralNetworksTested()
	  return (getPredictionPlotly(neuralNetwork.testResults.forEach.hiddenLayers, input$idSelect))
	})
	output$neuralNetworkForecastForAllChart <- renderPlotly({
	  neuralNetworksTested()
	  return (getPredictionPlotly(neuralNetwork.testResults.forAll, input$idSelect))
	})
	output$neuralNetworkForecastForAllHiddenChart <- renderPlotly({
	  neuralNetworksTested()
	  return (getPredictionPlotly(neuralNetwork.testResults.forAll.hiddenLayers, input$idSelect))
	})
	
	
	
	

	arModel <- reactive({
	  dataset = dataNormalized()
	  
	  if(is.null(dataset))
	  {
	    return(NULL)
	  }
	  getARModel(input$idSelect, data.normalized[[input$idSelect]]$y, input$windowSizeSlider, input$horizonSlider, input$aRModelName)
	})
	
	output$aRChart <- renderPlotly({
	    if(is.null(arModel())) return(NULL)

  	  getPlotlyModel()
	  })

	
	output$arMLE <- renderDataTable({
	  if(is.null(arModel())) return(NULL) 

	  error_metric(model$expected, model$result)
	})
	
	
	output$arCoef <- renderDataTable({
	  if(is.null(arModel())) return(NULL) 
	  
	  data.table(coef = model$coef)
	})
	
	
	
	
	
	compareError <- reactive({
	  if(is.null(dataset())) return(NULL)
	  neuralNetworksTested()
	  comarision(input$windowSizeSlider, input$horizonSlider)
	})
	
	output$compareMSE <- renderPlotly({
	  dataset = database()
	  
	  if(is.null(dataset))
	  {
	    return(NULL)
	  }
	  compareError()
	  getBoxplot('MSE')
	})
	
	output$compareRMSE <- renderPlotly({
	  dataset = database()
	  
	  if(is.null(dataset))
	  {
	    return(NULL)
	  }
	  compareError()
	  getBoxplot('RMSE')
	})
	
	output$compareSMAPE <- renderPlotly({
	  dataset = database()
	  
	  if(is.null(dataset))
	  {
	    return(NULL)
	  }
	  compareError()
	  getBoxplot('SMAPE')
	})
	
	
	output$compareError <- renderDataTable({
	  
	    error_metric_compare()
	})
	
	

	output$ErrorMetricTable <- renderDataTable({
	  result <- neuralNetworkTest()
	  error_metric(result$net.result[,1], result$net.expected, result$net.mse)
	})

	output$arACF <- renderPlot({
	  db = dataset()
	  if(is.null(db))
	  {
	    return(NULL)
	  }
	  plotACF(db$y)
	})
	
	output$arPACF <- renderPlot({
	  db = dataset()
	  if(is.null(db))
	  {
	    return(NULL)
	  }
	  plotPACF(db$y)
	})
	
}
