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

getPredictionPlotly <- function(id, forAll = FALSE, hiddenLayers = FALSE) {
  if (is.null(id))
  {
    return(NULL)
  }
  
  testResults <- getNeuralNetworkTestResults(id, forAll, hiddenLayers)
  if (is.null(testResults))
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
  rawData <- reactive({
    file <- input$dataFile
    
    if (is.null(file))
    {
      return(NULL)
    }
    
    #read.table(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    read.csv(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
  })

  database <- reactive({
    data <- rawData()

    if (!is.null(data))
    {
      parseData(data, idName = input$idColumnSelect, xName = input$x_axis, yName = input$y_axis)
      
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
	
	windowsCreated <- eventReactive(input$ButtonClick, {
	  dataNormalized()
	  windowSize <- input$windowSizeSlider
	  horizon <- input$horizonSlider
	  
	  if (!is.null(data.normalized))
	  {
	    createWindows(windowSize, horizon)
	    resetNeuralNetworks()
	    
	    list(trainSets = data.trainSets, testSets = data.testSets)
	  }
	})
	
	windowSplit <- reactive({
	  windowsCreated()
	  id <- input$idSelect
	  
	  list(trainset = data.trainSets[[id]], testset = data.testSets[[id]])
	})

	neuralNetworksTrained <- reactive({
	  windowsCreated()
	  
	  resetNeuralNetworks.hidden()
	  trainNeuralNetworks(input$biasCheckbox, c(input$hiddenSliderInput))
	  
	  list(forEach = neuralNetwork.forEach, forAll = neuralNetwork.forAll)
	})
	
	neuralNetworksTested <- reactive({
	  neuralNetworksTrained()
	  
	  testNeuralNetworks()
	  
	  list(neuralNetwork.testResults.forEach, neuralNetwork.testResults.forEach.hiddenLayers,
	    neuralNetwork.testResults.forAll, neuralNetwork.testResults.forAll.hiddenLayers)
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
	
	output$dataTable <- renderDataTable({
	  databaseChanged()
	  data.sets[[input$idSelect]]
	})
	output$trainDataTable <- renderDataTable({
	  windowsChanged()
	  getTrainSet(input$idSelect)
	})
	output$testDataTable <- renderDataTable({
	  windowsChanged()
	  getTestSet(input$idSelect)
	})
  
	
	
	output$neuralNetworkChart <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
		plot(getNeuralNetwork(input$idSelect), rep = 'best')
	})
	
	output$neuralNetworkHiddenChart <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  plot(getNeuralNetwork(input$idSelect, hiddenLayers = TRUE), rep = 'best')
	})
	
	output$neuralNetworkChartForAll <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  plot(getNeuralNetwork(NULL), rep = 'best')
	})
	
	output$neuralNetworkHiddenChartForALL <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  plot(getNeuralNetwork(NULL, hiddenLayers = TRUE), rep = 'best')
	})
	
	output$neuralNetworkForecastForEachChart <- renderPlotly({
	  #neuralNetworksTested()
	  return (getPredictionPlotly(input$idSelect))
	})
	output$neuralNetworkForecastForEachHiddenChart <- renderPlotly({
	  #neuralNetworksTested()
	  return (getPredictionPlotly(input$idSelect, hiddenLayers = TRUE))
	})
	output$neuralNetworkForecastForAllChart <- renderPlotly({
	  #neuralNetworksTested()
	  return (getPredictionPlotly(input$idSelect, forAll = TRUE))
	})
	output$neuralNetworkForecastForAllHiddenChart <- renderPlotly({
	  #neuralNetworksTested()
	  return (getPredictionPlotly(input$idSelect, forAll = TRUE, hiddenLayers = FALSE))
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
	
	
	
	
	
	### Settings Changed Events
	### Remember to set the output output option suspendwhenHidden to FALSE
	
	databaseChanged <- reactive({
	  rawData()
	  input$idColumnSelect
	  input$x_axis
	  input$y_axis
	  
	  resetNeuralNetworks()
	})
	output$databaseChanged <- reactive(databaseChanged())
	outputOptions(output, 'databaseChanged', suspendWhenHidden = FALSE)
	
	windowsChanged <- reactive({
	  data.windowSize <<- input$windowSizeSlider
	  data.horizon <<- input$horizonSlider
	  
	  resetWindows()
	  resetNeuralNetworks()
	  setNeuralNetworkExcludeVector()
	})
	output$windowsChanged <- reactive(windowsChanged())
	#outputOptions(output, 'windowsChanged', suspendWhenHidden = FALSE)
	
	excludeBiasChanged <- reactive({
	  neuralNetwork.excludeBias <<- input$biasCheckbox
	  
	  resetNeuralNetworks()
	})
	output$excludeBiasChanged <- reactive(excludeBiasChanged())
	
	hiddenLayersChanged <- reactive({
	  neuralNetwork.hiddenLayers <<- c(input$hiddenSliderInput)
	  
	  resetNeuralNetworks.hidden()
	})
	output$hiddenLayersChanged <- reactive(hiddenLayersChanged())
	outputOptions(output, 'hiddenLayersChanged', suspendWhenHidden = FALSE)
	
	
	
	
	
	### UI elements
	
	output$idColumnSelect <- renderUI({
	  df <- rawData()
	  if (is.null(df)) return()
	  columns <- names(df)
	  selectInput('idColumnSelect', 'ID Name', columns, selected = columns[1])
	})
	
	output$x_axis <- renderUI({
	  df <- rawData()
	  if (is.null(df)) return()
	  columns <- names(df)
	  selectInput("x_axis", "x-axis", columns, selected = columns[2])
	})
	
	output$y_axis <- renderUI({
	  df <- rawData()
	  if (is.null(df)) return()
	  columns <- names(df)
	  selectInput("y_axis","y-axis", columns, selected = columns[3])
	})
	
	output$idSelectBox <- renderUI({
	  database()
	  
	  if (!is.null(data.sets))
	  {
	    selectInput("idSelect", "Dataset", names(data.sets))
	  }
	})
	
	output$windowSizeSlider <- renderUI({
	  database()
	  id <- input$idSelect
	  
	  if (is.null(data.sets) || is.null(id))
	  {
	    return(NULL)
	  }
	  numData <- length(data.sets[[id]]$x)
	  sliderInput('windowSizeSlider', 'Window Size', 1, 0.05*numData, 0.0175*numData, step = 1)
	})
	
	output$horizonSlider <- renderUI({
	  windowSize <- input$windowSizeSlider
	  
	  if(!is.null(windowSize))
	  {
	    sliderInput('horizonSlider', 'Predict Values', 1, 2*windowSize, windowSize, step = 1)
	  }
	})
	
	output$hiddenSliderInput <- renderUI({
	  if (is.null(input$windowSizeSlider)) return()
	  sliderInput("hiddenSliderInput", "Number Hidden Neurons", 1, input$windowSizeSlider, 3, step = 1)
	})
}
