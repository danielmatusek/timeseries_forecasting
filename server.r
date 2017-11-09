library(forecast)
library(neuralnet)
library(zoo)

source('windows.r')
source('autoRegression.r')
source('neuralNetwork.r')

server <- function(input, output) {

  database_basis <- reactive({
    file <- input$dataFile

    if (is.null(file))
    {
      return(NULL)
    }
    
    df <- read.csv(file$datapath, header=input$headerCheckbox, sep=input$separatorRadioButton)
  })

	database <- reactive({
		df <- database_basis()
		if(is.null(df)) return(NULL)
		split(df, f = df$meterid)
	})

	dataset <- reactive({
		db <- database()
		meterid <- input$meteridSelect
		normalization <- input$normalizationRadioButton

		if (is.null(db) || is.null(meterid))
		{
			return(NULL)
		}

		df <- db[[meterid]]

		if (normalization == 'zScore')
		{
			data.frame(day = df[,x()], consumption = scale(df[,y()]))
		}
		else if (normalization == 'minmax')
		{
			maxs <- max(df[,y()])
			mins <- min(df[,y()])

			data.frame(day = df[,x()], consumption = scale(df[,y()], center = mins, scale = maxs - mins))
		}
		else
		{
			data.frame(day = df[,x()], consumption = df[,y()])
		}
	})
	
	output$x_axis <- renderUI({ 
	  df <- database_basis()
	  if (is.null(df)) return()
	  selectInput("x_axis", "x-Axis", names(df), selected = names(df)[2])
	})
	
	output$y_axis <- renderUI({
	  df <- database_basis()
	  if (is.null(df)) return()
	  selectInput("y_axis","y-Axis", names(df), selected = names(df)[3])
	})
	
	x <- reactive({ input$x_axis })
	
	y <- reactive({ input$y_axis })
	
	output$windowSizeSlider <- renderUI({
	  db <- database()
	  meterid <- input$meteridSelect
	  
	  if (is.null(db) || is.null(meterid))
	  {
	    return(NULL)
	  }
	  df <- db[[meterid]]
	  sliderInput('windowSizeSlider', 'Window Size', 1, max(df[,x()])*0.1, max(df[,x()])*0.05, step = 1)
	})	
	
	
	windows <- reactive({
	  df <- dataset()
	  windowSize <- input$windowSizeSlider
	  
	  if (is.null(df))
	  {
	    return(NULL)
	  }
	  
	  d <- as.data.frame(rollapply(df[,y()], width = windowSize+1, FUN = identity, by = 1), by.column = TRUE)
	  names(d) <- paste0('xt', 0:windowSize)
	  d
	})
	
	windowSplit <- reactive({
	  ws <- windows()
	  dataSplitFactor <- input$dataSplitSlider;
	  
	  if (is.null(ws))
	  {
	    return(NULL)
	  }
	  
	  splitWindows(ws, dataSplitFactor / 100)
	})

	neuralNetwork <- reactive({
		nnData = windowSplit()

		if (is.null(nnData))
		{
			return(NULL)
		}
		
		trainNeuralNetwork(nnData$trainset)
	})
	
	neuralNetworkTest <- reactive({
	  db <- database()
	  network <- neuralNetwork()
	  windows = windowSplit()
	  meterid <- input$meteridSelect
	  normalization <- input$normalizationRadioButton
	  
	  if (is.null(network))
	  {
	    return(NULL)
	  }
	  
	  scale <- 1
	  offset <- 0
	  if (normalization == 'zScore')
	  {
	    df <- db[[meterid]]
	    scale <- sd(df$consumption)
	    offset <- mean(df$consumption)
	  }
	  else if (normalization == 'minmax')
	  {
	    df <- db[[meterid]]
	    maxs <- max(df$consumption)
	    mins <- min(df$consumption)
	    
	    scale <- maxs - mins
	    offset <- mins
	  }
	  
	  testNeuralNetwork(network, windows$testset, scale, offset)
	})
	
	neuralNetworkCrossValidation <- reactive({
	  db <- database()
	  ws <- windows()
	  dataSplitFactor <- input$dataSplitSlider;
	  meterid <- input$meteridSelect
	  normalization <- input$normalizationRadioButton
	  
	  if (is.null(ws))
	  {
	    return(NULL)
	  }
	  
	  # Calculate Scale and Offset
	  scale <- 1
	  offset <- 0
	  if (normalization == 'zScore')
	  {
	    df <- db[[meterid]]
	    scale <- sd(df[,y()])
	    offset <- mean(df[,y()])
	  }
	  else if (normalization == 'minmax')
	  {
	    df <- db[[meterid]]
	    maxs <- max(df[,y()])
	    mins <- min(df[,y()])
	    
	    scale <- maxs - mins
	    offset <- mins
	  }
	  
	  # Cross Validation
	  mse <- NULL
	  for (i in 1:10)
	  {
	    windows <- splitWindows(ws, dataSplitFactor / 100)
	    network <- trainNeuralNetwork(windows$trainset)
	    results <- testNeuralNetwork(network, windows$testset, scale, offset)
	    mse[i] <- results$net.mse
	  }
	  
	  mse
	})

	output$meteridSelectBox <- renderUI({
		db <- database()

		if (is.null(db))
		{
			return(NULL)
		}

		selectInput("meteridSelect", "Dataset", names(db))
	})

	output$dataChart <- renderPlotly({
		df <- dataset()

		if (is.null(df))
		{
			return(NULL)
		}

		p <- plot_ly(df, x = df[,x()], y = df[,y()], type = 'scatter', mode = 'lines')
		p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
		p
	})
	
	output$dataTable <- renderDataTable(dataset())
	output$trainDataTable <- renderDataTable(windowSplit()$trainset)
	output$testDataTable <- renderDataTable(windowSplit()$testset)
  
	
	
	output$neuralNetworkChart <- renderPlot({
		nn <- neuralNetwork()

		if (is.null(nn))
		{
			return(NULL)
		}

		plot(nn, rep = "best")
	})
	
	output$neuralNetworkTestResultsTable <- renderDataTable({
	  result <- neuralNetworkTest()
	  data.frame(expected = result$net.expected, result = result$net.result)
	})
	
	output$neuralNetworkTestResultChart <- renderPlot({
	  result <- neuralNetworkTest()
	  
	  if (is.null(result))
	  {
	    return(NULL)
	  }
	  
	  plot(result$net.expected, result$net.result, col = 'red', main='Real vs predicted NN', pch=18, cex=0.7)
	  abline(0, 1, lwd = 2)
	  mtext(paste('MSE = ', sum((result$net.expected - result$net.result)^2)/nrow(result$net.result)))
	})
	
	output$neuralNetworkCrossValidationChart <- renderPlotly({
	  mse <- neuralNetworkCrossValidation()
	  
	  if (is.null(mse))
	  {
	    return(NULL)
	  }
	  
	  p <- plot_ly(x = mse, type = 'box', pointpos = -1.8, boxpoints = 'all', name = 'MSE')
	  p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
	  p
	})
	
	output$neuralNetworkCrossValidationTable <- renderDataTable(data.frame(MSE = neuralNetworkCrossValidation()))

	aRModel <- reactive({
	    meterid <- input$meteridSelect
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
	    
	    
	    
	    df = db$consumption
	    winSize = input$windowSizeSlider
	    numPredictValues = input$dataPrediction
	    
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
	  
	  
	  df = db$consumption
	  
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
