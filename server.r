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
source('recNeuralNetwork.r')
source('plot.rsnns.r')

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB
cpu_time <- list()

server <- function(input, output) {
  
  ### Settings Changed Events
  
  rawData <- reactive({
    file <- input$dataFile
    if (is.null(file) && input$use_data=="csv")
    {
      return(NULL)
    }
    #read.table(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    if(input$use_data=="alipay"){
      data  <- readRDS("../resources/alipay_base.rdata")
    } 
    if (input$use_data=="metadata"){
      data  <- readRDS("../resources/meterdata_complete_series.RData")
    }
    if (input$use_data=="csv"){
      data  <- read.csv(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    }
    
    return(data)
  })
  

  databaseChanged <- reactive({
    data <- rawData()

    if (!is.null(data))
    {
      parseData(data, idName = input$idColumnSelect, xName = input$x_axis, yName = input$y_axis)
    }
    resetNeuralNetworks()
  })
	
	windowsChanged <- reactive({
	  data.windowSize <<- input$windowSizeSlider
	  data.horizon <<- input$horizonSlider
	  excludeInputChanged()
	  resetWindows()
	  resetARModels()
	  resetComparison()
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	
	idChanged <- reactive({
	  data.idSelected <<- input$idSelect
	  resetNeuralNetworks.InputExclusion()
	})
	
	
	
	excludeBiasChanged <- reactive({
	  neuralNetwork.excludeBias <<- input$biasCheckbox
	  
	  resetARModels()
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	hiddenLayersChanged <- reactive({
	  neuralNetwork.hiddenLayers <<- c(input$hiddenSliderInput)
	  
	  resetNeuralNetworks.hidden()
	  resetNeuralNetworks.InputExclusion()
	})
	
	excludeInputChanged <- reactive({
	  
	  neuralNetwork.isInputExcluded <<- input$inputCheckbox
	  
	  if(neuralNetwork.isInputExcluded == TRUE)
	  {
	    neuralNetwork.excludedMaxInputs <<- input$hiddenSliderInput
	    neuralNetwork.excludedInput <<- input$hiddenSliderInput
	  }
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	inputStrategyChanged <- reactive({
	  return(NULL)
	})
	
	nnTypChanged <- reactive({
	  neuralNetwork.enableForEach <<- 'forecast_one' %in% input$variable_nn
	  neuralNetwork.enableForEach.hidden <<- 'forecast_one_hidden' %in% input$variable_nn
	  neuralNetwork.enableForAll <<- 'forecast_all' %in% input$variable_nn
	  neuralNetwork.enableForAll.hidden <<- 'forecast_all_hidden' %in% input$variable_nn
		rsnns.rnn <<- 'rsnns_rnn' %in% input$variable_nn
		rsnns.mlp <<- 'rsnns_mlp' %in% input$variable_nn
		resetComparison()
	})
	
	arModelBaseChanged <- reactive({
	  aRModelName <<- input$aRModelName
	  
	  resetARModels()
	})
	
	errorTypChanged <- reactive({
	  idChanged()
	  resetComparison()
	  errorTypCheck <<- input$errorTypCheck
	})
	
	
	
	### UI elements: General
	
	
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
	  databaseChanged()
	  
	  if (!is.null(data.sets))
	  {
	    d <- data.sets[order(as.numeric(names(data.sets)))]   #sortieren von names(data.sets)
	    selectInput("idSelect", "Dataset", names(d))
	  }
	})
	
	output$windowSizeSlider <- renderUI({
	  databaseChanged()
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
	
	output$excludeInputSlider <- renderUI({
	  if(input$inputCheckbox == TRUE)
	  {
	    if(input$windowSizeSlider > 1)
	    {
	      #sliderInput("excludeInputSlider", "Exclude Max Inputs", 1, input$windowSizeSlider - 1, 3, step = 1) 
	      selectInput("inputStrategy", "Strategy", neuralnetwork.strategies)
	    }
	    
	  }
	  
	})
	
	
	### UI elements: Data
	
	
	output$dataChart <- renderPlotly({
	  databaseChanged()
	  
		p <- plot_ly(data.sets[[input$idSelect]], x = ~x, y = ~y, type = 'scatter', mode = 'lines')
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
	
	
	
	
	
	### UI elements: Neural Network
	
	
	output$neuralNetwork_tabs <- renderUI({
	  excludeInputChanged()
	  panels <- list()
	  
	 	myTabs <- lapply(input$variable_nn, function(x){
	 	  
	    if(x == "forecast_one")
	    {
	      panels[[length(panels) + 1]] <- tabPanel('Forecast /1', 
	                                             plotOutput("neuralNetworkChart", height = "600px")) 
	    }
	    else if(x == "forecast_one_hidden")
	    {
	      panels[[length(panels) + 1]] <- tabPanel('Forecast /1 hidden', 
	                                             plotOutput("neuralNetworkHiddenChart", height = "600px"))
	    }
	    else if(x == "forecast_all"){
	      panels[[length(panels) + 1]] <- tabPanel('Forecast /n', 
	                                             plotOutput("neuralNetworkChartForAll", height = "600px"))
	    } 
	    else if(x == "forecast_all_hidden")
	    {
	      panels[[length(panels) + 1]] <- tabPanel('Forecast /n hidden',
	                                             plotOutput("neuralNetworkHiddenChartForALL", height = "600px"))
	    }
	  })
	 	
	 	
	 	myTabs <- c(myTabs, lapply(input$variable_nn, function(x){
	 	  
	 	 if(neuralNetwork.isInputExcluded)
	 	 {
	 	    if(x == "forecast_one" )
	 	    {
  	 	   panels[[length(panels) + 1]] <- tabPanel('Forecast /1 Ex. Inp.',
  	 	                                            plotOutput("nncei", height = "600px"),
  	 	                                            dataTableOutput("nneidt"))
	 	    }
	 	    else if(x == "forecast_one_hidden")
	 	    {
	 	      panels[[length(panels) + 1]] <- tabPanel('Forecast /1 hidden Ex. Inp.',
	 	                                               plotOutput("nnhcei", height = "600px"),
	 	                                               dataTableOutput("nnheidt"))
	 	    }
	 	   else if(x == "forecast_all")
	 	   {
	 	     panels[[length(panels) + 1]] <- tabPanel('Forecast /n Ex. Inp.',
	 	                                              plotOutput("nnfacei", height = "600px"),
	 	                                              dataTableOutput("nnfaeidt"))
	 	   }
	 	   else if(x == "forecast_all_hidden")
	 	   {
	 	     panels[[length(panels) + 1]] <- tabPanel('Forecast /n hidden Ex. Inp.',
	 	                                              plotOutput("nnhfacei", height = "600px"),
	 	                                              dataTableOutput("nnhfaeidt"))
	 	   }
	   }
	 	}))
	 	
	  myTabs$width = "100%"
	  do.call(tabBox, myTabs)
	})
	
	output$neuralNetwork_hlOptimization <- renderUI({
	  panels <- list()
	  myhlOptimizationTabs <- lapply(input$variable_nn_hidden, function(x){
			if(x == "optimize_hidden_layer"){
	      panels[[length(panels)+1]] <- tabPanel('HiddenLayer Trial and Error', 
	                                             plotOutput("neuralNetworkChartHiddenTrialError", height = "600px"),
	                                             h5(textOutput(('neuralNetworkForecastForTrialError'))),
																							 dataTableOutput("neuralNetworkTableForTrialError")
	      )
			}
		})
	  myhlOptimizationTabs$width = "100%"
	  do.call(tabBox, myhlOptimizationTabs)
	})
	
	output$neuralNetworkChart <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  
		plot(getNeuralNetwork(input$idSelect, FALSE), rep = 'best')
	})
	
	output$neuralNetworkHiddenChart <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getNeuralNetwork(input$idSelect, TRUE), rep = 'best')
	})
	
	output$neuralNetworkChartForAll <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  
	  plot(getNeuralNetwork(NULL, FALSE), rep = 'best')
	})
	
	output$neuralNetworkHiddenChartForALL <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getNeuralNetwork(NULL, TRUE), rep = 'best')
	})
	
	
	
	output$nncei <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  
	  plot(getExcludedInputNeuralNetwork(input$idSelect, hiddenLayers = FALSE, input$inputStrategy), rep = 'best')
	})
	
	output$nnhcei <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getExcludedInputNeuralNetwork(input$idSelect, TRUE, input$inputStrategy), rep = 'best')
	})
	
	output$nnfacei <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  
	  plot(getExcludedInputNeuralNetwork(NULL, FALSE, input$inputStrategy), rep = 'best')
	})
	
	output$nnhfacei <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getExcludedInputNeuralNetwork(NULL, TRUE, input$inputStrategy), rep = 'best')
	})
	
	
	
	output$nneidt <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
    s <- neuralNetwork.excluded.statistics[[2]]
    data.table("Excluded Nodes" = s$nodes, sMAPE = s$smape, "Sampling Error" = s$internalE)
	})
	
	output$nnheidt <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  
	  s <- neuralNetwork.excluded.statistics[[4]]
	  data.table("Excluded Nodes" = s$nodes, sMAPE = s$smape, "Sampling Error" = s$internalE)
	})
	
	output$nnfaeidt <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  s <- neuralNetwork.excluded.statistics[[1]]
	  data.table("Excluded Nodes" = s$nodes, sMAPE = s$smape, "Sampling Error" = s$internalE)
	})
	
	output$nnhfaeidt <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  s <- neuralNetwork.excluded.statistics[[3]]
	  data.table("Excluded Nodes" = s$nodes, sMAPE = s$smape, "Sampling Error" = s$internalE)
	})

	
	
	
	output$neuralNetworkChartHiddenTrialError <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  excludeBiasChanged()
	  inputStrategyChanged()
	  hiddenLayersChanged()
		optimizeNeuralNetworkHiddenLayer(input$idSelect)
	  
	  plot(getNeuralNetwork(input$idSelect, hlOptimization = TRUE), rep = 'best')
	})

	output$neuralNetworkForecastForTrialError <- renderPrint({
		windowsChanged()
		idChanged()
		neuralNetwork.hiddenLayers <<- neuralNetwork.tempHiddenNodes
		cat("Optimal number of Hidden nodes: ", neuralNetwork.hlOptimization, fill = FALSE)
	})

	output$neuralNetworkTableForTrialError <- renderDataTable({
		windowsChanged()
		idChanged()
		getHlOptimizationErrorTable(input$idSelect)
	})

	
	
	### UI elements: RSNNS Package

	#Recurrent Neural Network
	
	output$reccurentNeuralNetwork_tab <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		m <- trainRNN(input$idSelect, input$hiddenSliderInput)
		t <- testRNN(m, input$idSelect)
		
		data.table(result = t$result, expected = t$expected)		
	})

	#MLP with RSNNS and with Hidden Layer

	output$rsnns_mlp_tab <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		m <- trainMLP(input$idSelect)
		t <- testMLP(m, input$idSelect)
		
		data.table(result = t$result, expected = t$expected)		
	})

	#MLP with RSNSS and without Hidden Layer
	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		m <- trainMLP(input$idSelect, hiddenLayers = FALSE)
		t <- testMLP(m, input$idSelect)
		
		data.table(result = t$result, expected = t$expected)		
	})

	output$recPlot <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(trainRNN(input$idSelect, input$hiddenSliderInput), paste0('xt', 1:data.windowSize))
	})

	output$mlp_plot <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(trainMLP(input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$mlp_plot_without_hidden <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(trainMLP(input$idSelect, hiddenLayers = FALSE), paste0('xt', 1:data.windowSize))
	})
	
	
	
	
	
	### UI elements: Auto Regression
	
	
	output$arACF <- renderPlot({
	  databaseChanged()
	  
	  acf(data.sets[[input$idSelect]]$y, main = "ACF")
	})
	
	output$arPACF <- renderPlot({
	  databaseChanged()
	  
	  pacf(data.sets[[input$idSelect]]$y, main = "PACF")
	})
	
	
	
	
	
	### UI elements: Comparision
	
	output$forecastComparisionPlot <- renderPlotly({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  
	  getForecastComparisionPlot(input$idSelect)
	})
	
	output$compareMSE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  
	  getBoxplot('mse')
	})
	
	output$compareRMSE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  
	  getBoxplot('rmse')
	})
	
	output$compareSMAPE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  
	  getBoxplot('smape')
	})
	
	output$compareError <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  
	  getErrorMetricCompare()
	})
	
	output$compareCoefficient <- renderDataTable({
	  idChanged()
	  databaseChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  nnTypChanged()
	  arModelBaseChanged()
	  
	  getCoef(input$idSelect)
	})
	
	output$neuralNetworkDifferenceWRTHiddenLayers <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  nnTypChanged()
	  
	  findDifferenceInNeuralNetworksWrtHiddenLayers()
	})
	
	output$data_cpu_time <- renderPlotly({
	  arModelBaseChanged()
	  windowsChanged()
	  
	  resetARModels()
	  resetNeuralNetworks()

	  nnTypChanged()
    x <- list()
	  if(neuralNetwork.enableForEach){
	    cpu_time[1] <- system.time(getNeuralNetwork(input$idSelect))[3] +0
	    x <- c(x,"one modell")
	  } 
	  if(neuralNetwork.enableForEach.hidden){
	    cpu_time[2] <- system.time(getNeuralNetwork(input$idSelect, hiddenLayers = TRUE))[3] +0
	    x <- c(x,"one modell hidden") 
	  } 
	  if(neuralNetwork.enableForAll){
	    cpu_time[3] <- system.time(getNeuralNetwork(NULL))[3] +0
	    x <- c(x,"all time series without hidden") 
	  } 
	  if(neuralNetwork.enableForAll.hidden){
	    cpu_time[4] <- as.numeric(system.time(getNeuralNetwork(NULL, hiddenLayers = TRUE))[3])*100
	    x <- c(x,"all time series with hidden") 
	  }     
    resetARModels()
#	  cpu_time[5] <- system.time(getARModel(input$idSelect))[3] +0
#	  resetARModels()
#   getARModel(input$idSelect)
    cpu_time[5] <- system.time(getARModel(input$idSelect))[3] +0
    x <- c(x,"AR")
    data <- unlist(cpu_time)
    
    #sort x
    x <- factor(x, levels = unique(x)[order(c(data), decreasing = TRUE)])
	  
	  p <- plot_ly(
	    x = x, #c("one modell", "one modell hidden","AR"),# "all time series without hidden","AR"),# "all time series with hidden"),
	    y = c(data),
	    type = "bar"
	  )
	  p
	  })
}
