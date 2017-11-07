library(shiny)
library(shinydashboard)
library(plotly)
library(neuralnet)
library(zoo)
library(ggplot2)
library(forecast)


options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB


ui <- dashboardPage(
	dashboardHeader(title = "FPADB"),

	dashboardSidebar(
		checkboxInput('headerCheckbox', 'Header', TRUE),
		radioButtons('separatorRadioButton', 'Separator',
			c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),
		fileInput('dataFile', NULL,
			accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
		),

		hr(),

		uiOutput("meteridSelectBox"),
		radioButtons('normalizationRadioButton', 'Normalization',
			c('None' = 'none', 'Z-Normalization' = 'zScore', 'Min-Max Scale' = 'minmax'), 'none'),
		sliderInput('windowSizeSlider', 'Window Size', 1, 30, 5, step = 1),
		sliderInput('dataSplitSlider', 'Split Training/Test Data', 1, 100, 70, step = 1),
		sliderInput('dataPrediction', 'Predict Values', 1, 50, 10, step = 1),

		hr(),

		sidebarMenu(id="tabs",
			menuItem("Data", tabName = "data", icon = icon("database")),
			menuItem("Neural Network", tabName = "neuralNetwork", icon = icon("sitemap", "fa-rotate-90")),
			menuItem("Autoregressive", tabName = "aRModel", icon = icon("table"))
			
		)
	),

	dashboardBody(
		tabItems(
			tabItem(tabName = "data",
				tabBox(width = NULL,
					tabPanel("Chart",
						plotlyOutput("dataChart", height = "600px")
					),
					tabPanel("Table",
						dataTableOutput("dataTable")
					),
				  tabPanel('Train Data',
  				  dataTableOutput('trainDataTable')
          ),
				  tabPanel('Test Data',
  				  dataTableOutput('testDataTable')
				  )
				)
			),

			tabItem(tabName = "neuralNetwork",
				tabBox(width = NULL,
					tabPanel("Chart",
						plotOutput("neuralNetworkChart", height = "600px")
					),
					tabPanel("Test Results",
						dataTableOutput("neuralNetworkTestResultsTable")
					),
				  tabPanel('Result Chart',
				    plotOutput('neuralNetworkTestResultChart')
				  ),
				  tabPanel('Cross Validation',
				    plotlyOutput('neuralNetworkCrossValidationChart'),
				    dataTableOutput('neuralNetworkCrossValidationTable')
				  )
				)
			),
			
			tabItem(tabName = "aRModel",
			        tabBox(width = NULL,
			               tabPanel("Chart",
			                        plotlyOutput("aRChart", height = "600px")
			               ),
			               tabPanel("Forecast",
			                        plotlyOutput("aRCForecast", height = "600px")
			               ),
			               tabPanel("Test Results", dataTableOutput("ARResultsTable"))
			        )
			)
		)
	)
)

splitWindows <- function(windows, splitFactor) {
  index <- 1:nrow(windows)
  trainindex <- sample(index, trunc(length(index) * splitFactor))
  trainset <- windows[trainindex, ]
  testset <- windows[-trainindex, ]
  
  list(trainset = trainset, testset = testset)
}

trainNeuralNetwork <- function(trainset) {
  n <- names(trainset)
  f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))
  
  neuralnet(f, trainset, hidden = 0, linear.output = TRUE)
}

testNeuralNetwork <- function(neuralNetwork, testset, scale, offset) {
  expected <- testset$xt0
  testset$xt0 <- NULL
  
  n <- compute(neuralNetwork, testset)
  
  n$net.result <- n$net.result * scale + offset
  n$net.expected <- expected * scale + offset
  n$net.mse <- sum((n$net.expected - n$net.result)^2)/nrow(n$net.result)
  
  n
}

server <- function(input, output) {
  
	database <- reactive({
		file <- input$dataFile

		if (is.null(file))
		{
			return(NULL)
		}

		df <- read.csv(file$datapath, header=input$headerCheckbox, sep=input$separatorRadioButton)
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
			data.frame(day = df$day, consumption = scale(df$consumption))
		}
		else if (normalization == 'minmax')
		{
			maxs <- max(df$consumption)
			mins <- min(df$consumption)

			data.frame(day = df$day, consumption = scale(df$consumption, center = mins, scale = maxs - mins))
		}
		else
		{
			data.frame(day = df$day, consumption = df$consumption)
		}
	})
	
	windows <- reactive({
	  df <- dataset()
	  windowSize <- input$windowSizeSlider
	  
	  if (is.null(df))
	  {
	    return(NULL)
	  }
	  
	  d <- as.data.frame(rollapply(df$consumption, width = windowSize+1, FUN = identity, by = 1), by.column = TRUE)
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

		p <- plot_ly(df, x = ~day, y = ~consumption, type = 'scatter', mode = 'lines')
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

shinyApp(ui, server)
