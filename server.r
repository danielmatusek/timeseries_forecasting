library(forecast)
library(data.table)
library(neuralnet)
library(zoo)

source('autoRegression.r')
source('neuralNetwork.r')

server <- function(input, output) {

  database <- reactive({
    file <- input$dataFile

    if (is.null(file))
    {
      return(NULL)
    }
    
    #read.table(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    data <- read.csv(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    parseData(data, xName = input$x_axis, yName = input$y_axis)
    
    data.sets
  })
  
  dataNormalized <- reactive({
    database()
    normalizationMethod <- input$normalizationRadioButton
    
    if (is.null(data.sets) || is.null(normalizationMethod))
    {
      return(NULL)
    }
    
    normalizeData(normalizationMethod)
    
    data.normalized
  })

	dataset <- reactive({
	  dataNormalized()
		id <- input$idSelect

		if (is.null(data.normalized) || is.null(id))
		{
			return(NULL)
		}
		
		data.normalized[[id]]
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
	  horizon <- input$horizon
	  
	  if (!is.null(data.normalized))
	  {
	    createWindows(windowSize, horizon)
	  }
	  
	  list(trainset = data.trainSets, testset = data.testSets)
	})
	
	windowSplit <- reactive({
	  windowsCreated()
	  id <- input$idSelect
	  
	  if (!is.null(data.testSets))
	  {
	    list(trainset = data.trainSets[[id]], testset = data.testSets[[id]])
	  }
	})

	neuralNetworksLearned <- reactive({
	  windowsCreated()

		if (!is.null(data.trainSets))
		{
		  trainNeuralNetworks()
		}
	  
	  list(forEach = neuralNetwork.forEach, forAll = neuralNetwork.forAll)
	})
	
	neuralNetworkTest <- reactive({
	  database()
	  db <- data.sets
	  network <- neuralNetwork()
	  windows = windowSplit()
	  id <- input$idSelect
	  normalization <- input$normalizationRadioButton
	  
	  if (is.null(network))
	  {
	    return(NULL)
	  }
	  
	  scale <- 1
	  offset <- 0
	  if (normalization == 'zScore')
	  {
	    df <- db[[id]]
	    scale <- sd(df$y)
	    offset <- mean(df$y)
	  }
	  else if (normalization == 'minmax')
	  {
	    df <- db[[id]]
	    maxs <- max(df$y)
	    mins <- min(df$y)
	    
	    scale <- maxs - mins
	    offset <- mins
	  }
	  
	  testNeuralNetwork(network, windows$testset, scale, offset)
	})

	output$idSelectBox <- renderUI({
	  database()

		if (is.null(data.sets))
		{
			return(NULL)
		}

		selectInput("idSelect", "Dataset", names(data.sets))
	})

	output$dataChart <- renderPlotly({
		df <- dataset()

		if (is.null(df))
		{
			return(NULL)
		}

		p <- plot_ly(df, x = df$x, y = df$y, type = 'scatter', mode = 'lines')
		p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
		p
	})
	
	output$dataTable <- renderDataTable(dataset())
	output$trainDataTable <- renderDataTable(windowSplit()$trainset)
	output$testDataTable <- renderDataTable(windowSplit()$testset)
  
	
	
	output$neuralNetworkChart <- renderPlot({
	  neuralNetworksLearned()
	  id <- input$idSelect

		if (!is.null(neuralNetwork.forEach) && !is.null(id))
		{
		  plot(neuralNetwork.forEach[[id]], rep = "best")
		}
	})
	
	output$neuralNetworkCrossValidationChart <- renderPlotly({
	  mse <- neuralNetworkCrossValidation()
	  
	  if (is.null(mse))
	  {
	    return(NULL)
	  }
	  
	  p <- plot_ly(x = mse, type = 'box', name = 'MSE')
	  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
	  p
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
	    numPredictValues = input$horizon
	    browser()
	    
	    to = round(length(df) * dataSplitFactor)
	    trainData = df[1: to]
	    aRModel = arima(ts(trainData,start = 1, end = to), order= c(winSize,0,0))
	    forecast(aRModel, h = numPredictValues)
	  })
	
	output$aRChart <- renderPlotly({
	  
	    fc = aRModel()
	    
	    if (is.null(fc))
	    {
	      return(NULL)
	    }
	   
	    df = data.frame(fitted = fc$fitted, act = fc$x)
	    
	    p <- plot_ly(df , y = ~fitted, type ="scatter", name= "fitted", mode= "lines+markers")%>%
	      add_trace(y = ~act, name = 'actual', mode = 'lines+markers')
	    p$elementId <- NULL
	    p
	    
	  })

	output$ARResultsTable <- renderDataTable({
	  
	  fc = aRModel()
	  data.frame(expected = fc$x , result = fc$fitted)
	})

	output$aRCForecast <- renderPlotly({
	  
	  dataSplitFactor <- input$dataSplitSlider / 100;
	  fc = aRModel()
	  if (is.null(fc))
	  {
	    return(NULL)
	  }
	  
	  db <- dataset()
	  
	  if(is.null(db))
	  {
	    return(NULL)
	  }
	  
	  
	  df = db$y
	  
	  act = 0
	  numTrain = round(length(df) * dataSplitFactor)
	  
	  if((length(df) - numTrain) > input$dataPrediction)
	  {
	    act = db$consumption[numTrain+1 : input$dataPrediction]
	  }
	  else
	  {
	    act = rep(0, input$dataPrediction)
	  }
	  
	  df = data.frame(actual = act, forecast = fc$mean)
	  
	  p <- plot_ly(df , y = ~forecast, type ="scatter", name= "Forecast Values", mode= "lines+markers")%>%
	    add_trace(y = ~actual, name = 'Actual Values', mode = 'lines+markers')
	  p$elementId <- NULL
	  p
	  
	  
	})
}
