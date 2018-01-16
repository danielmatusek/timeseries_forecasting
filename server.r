library(data.table)
if (!require("DT")) install.packages('DT')
library(forecast)
library(hydroGOF) #rmse function
library(neuralnet)
library(stats)
library(TSPred) #sMape function
library(zoo)

source('autoRegression.r')
source('neuralNetwork.r', encoding = 'UTF-8')
source('comparision.r')
source('recNeuralNetwork.r')
source('plot.rsnns.r')

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB

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
	  resetWindows()
	  resetModels('ar')
	  resetComparison()
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	

	excludeBiasChanged <- reactive({
	  neuralNetwork.excludeBias <<- input$biasCheckbox
	  
	  resetModels('ar')
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	hiddenLayersChanged <- reactive({
	  neuralNetwork.hiddenLayers <<- c(input$hiddenSliderInput)
	  
	  resetNeuralNetworks.hidden()
	  resetNeuralNetworks.InputExclusion()
	})
	
	
	
	excludeInputErrorChanged <- reactive({
	  neuralnetwork.greedyErrorType <<- input$inputSelectedErrorType
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	
	enabledModelsChanged <- reactive({
	  vars$enabledModels <<- input$enabledModels
		resetComparison()
	})
	
	arModelBaseChanged <- reactive({
	  aRModelName <<- input$aRModelName
	  
	  resetModels('ar')
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
	  values <- input$windowSizeSlider
	  if(is.null(values)){
	    values <- 0.0175*numData
	  }
	  sliderInput('windowSizeSlider', 'Window Size', 1, round(0.05*numData), values, step = 1)
	})
	
	output$horizonSlider <- renderUI({
	  windowSize <- input$windowSizeSlider
	  
	  if(!is.null(windowSize))
	  {
	    values <- input$horizonSlider
	    if(is.null(values)){
	      values <- windowSize
	    }
	    sliderInput('horizonSlider', 'Predict Values', 1, 2*windowSize, values, step = 1)
	  }
	})
	
	output$hiddenSliderInput <- renderUI({
	  if (is.null(input$windowSizeSlider)) return()
	  values <- input$hiddenSliderInput
	  if(is.null(values)){
	    values <- 3
	  }
		maxHiddenSlider <- input$windowSizeSlider * 2
	  sliderInput("hiddenSliderInput", "Number Hidden Neurons", 1, maxHiddenSlider, values, step = 1)
	})
	
	
	
	output$inputStrategy <- renderUI({
	  
	  if (is.null(input$windowSizeSlider)) return()
	  if(input$inputCheckbox == TRUE && input$windowSizeSlider > 1)
	  {
	      selectInput("inputStrategy", "Strategy", neuralnetwork.strategies)
	  }
	})
	
	output$inputSelectedErrorType <- renderUI({
	  
	  if (is.null(input$windowSizeSlider)) return()

	 selectInput("inputSelectedErrorType", "Error Type", c("Outsample", "Insample"))
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
	  enabledModelsChanged()
	  
	  supportedNeuralNetworks <- c('nnfe', 'nnfeh', 'nnfa', 'nnfah', 'nnfeei','nnfehei','nnfed','nnfehd', 'nnfeeic', 'nnfeheic')
	  optimizedNeuralNetworks <- c('nnfeei','nnfehei', 'nnfeeic', 'nnfeheic')
	  enabledNeuralNetworks <- intersect(vars$enabledModels, supportedNeuralNetworks)
	  
	  tabs <- lapply(enabledNeuralNetworks, function(nnName) {
	    if (nnName %in% optimizedNeuralNetworks)
	    {
	      tabPanel(nnName,
	               plotOutput(paste0(nnName, 'Plot'), height = '600px'),
	               dataTableOutput(paste0(nnName, 'OptimizingTable'))
	      )
	    }
	    else
	    {
	      tabPanel(nnName, plotOutput(paste0(nnName, 'Plot'), height = '600px'))
	    }
	  })
	 	
	 	
	 	
	 tabs$width = "100%"
	 do.call(tabBox, tabs)
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
	
	output$nnfePlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()

		plot(getModel('nnfe', input$idSelect), rep = 'best')
	})
	
	output$nnfehPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getModel('nnfeh', input$idSelect), rep = 'best')
	})
	
	output$nnfaPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  
	  plot(getModel('nnfa'), rep = 'best')
	})
	
	output$nnfahPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getModel('nnfah'), rep = 'best')
	})
	
	
	# Plot Differntiable Inputs 
	
	output$nnfedPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	
	  plot(getModel('nnfed', input$idSelect), rep = 'best')
	})
	
	output$nnfehdPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  plot(getModel('nnfehd', input$idSelect), rep = 'best')
	})
	
	
	
	
	# Plot Exclude Inputs 
	
	output$nnfeeiPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfeei', input$idSelect)
	  plot(m$model, rep = 'best')
	})
	
	output$nnfeheiPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfehei', input$idSelect)
	  plot(m$model, rep = 'best')
	})
	
	
	# Plot Exluded Input Stats
	
	output$nnfeeicPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()

	  m <- getModel('nnfeeic', NULL)
	  plot(m$pExcluded, m$excludedPathCounter, type = "l", col ="green", xlab = "Excluded Node", ylab = "Number")

	})
	
	output$nnfeheicPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfeheic', NULL)
	  plot(m$pExcluded, m$excludedPathCounter, type = "l", col ="green", xlab = "Excluded Node", ylab = "Number")
	})
	
	# Exclude Inputs Datatable
	
	getExcludedInputTable <- function(modelName)
	{
	  s <- getModel(modelName, input$idSelect)
	  
	  dt <- data.table("Excluded Nodes" = s$nodes, "sMAPE" = s$smape, "Sampling Error" = s$internalE, info = s$info)
	  dt <- dt[rowSums(is.na(dt)) == 0,]
	  datatable(head(dt, 50),
	            class = 'cell-border stripe',
	            options = list(
	              columnDefs = list(list(targets = 4, visible = FALSE)),
	              pageLength = 50))%>%
	    formatStyle("info",target = 'row',color = "black", backgroundColor = styleEqual(c(0, 1,2), c('white', 'yellow','#00ff00')), fontWeight = styleEqual(c(2), c('bold')))
	  
	}
	

	output$nnfeeiOptimizingTable <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable('nnfeei')
	})
	

	output$nnfeheiOptimizingTable <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable('nnfehei')
	})
	
	# Data table for excluded Input stats
	output$nnfeeicOptimizingTable <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfeeic')
	  
	  data.table('id' = c(m$ids, m$pathsCombinedE), 'path' = c(m$paths, m$pathsCombinedN))
	})
	
	output$nnfeheicOptimizingTable <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfeheic')
	  data.table('id' = m$ids, 'path' = m$paths)
	})
	

	
	
	
	output$neuralNetworkChartHiddenTrialError <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
		optimizeNeuralNetworkHiddenLayer(input$idSelect)
	  
	  plot(getNeuralNetwork(input$idSelect, hlOptimization = TRUE), rep = 'best')
	})

	output$neuralNetworkForecastForTrialError <- renderPrint({
		windowsChanged()
		neuralNetwork.hiddenLayers <<- neuralNetwork.tempHiddenNodes
		cat("Optimal number of Hidden nodes: ", neuralNetwork.hlOptimization, fill = FALSE)
	})

	output$neuralNetworkTableForTrialError <- renderDataTable({
		windowsChanged()
		getHlOptimizationErrorTable(input$idSelect)
	})

	
	
	### UI elements: RSNNS Package

	#Recurrent Neural Network
	
	output$reccurentNeuralNetwork_tab <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('elman', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	#MLP with RSNNS and with Hidden Layer

	output$rsnns_mlp_tab <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlp', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	#MLP with RSNSS and without Hidden Layer
	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlp', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$recPlot <- renderPlot({
  	windowsChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('elman', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlph', input$idSelect)
		
		data.table(result = t$result, expected = t$expected)
	})

	output$mlp_plot <- renderPlot({
  	windowsChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('mlp', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlph', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$mlp_plot_without_hidden <- renderPlot({
  	windowsChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('mlph', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_jordan_tab <- renderDataTable({
		windowsChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('jordan', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$rsnns_jordan_plot <- renderPlot({
  	windowsChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()

		plot(getModel('jordan', input$idSelect), paste0('xt', 1:data.windowSize))
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
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  excludeInputErrorChanged()
	  arModelBaseChanged()
	  
	  getForecastComparisionPlot(input$idSelect)
	})
	
	output$errorMetricPlot <- renderPlotly({
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  
	  getModelErrorPlot(input$errorMetricName, if(input$errorOfAllTimeseries) { NULL } else { input$idSelect })
	})
	
	output$compareError <- renderDataTable({
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  
	  getErrorMetricCompare()
	})
	
	output$compareCoefficient <- renderDataTable({
	  databaseChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  
	  getCoef(input$idSelect)
	})
	
	output$neuralNetworkDifferenceWRTHiddenLayers <- renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  
	  findDifferenceInNeuralNetworksWrtHiddenLayers()
	})
	
	modelComparision <- eventReactive(input$compareModels, {
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  
	  if (is.null(data.sets))
	  {
	    return (NULL)
	  }
	  
	  compareModels(input$model1Select, input$model2Select, 0.02)
	})
	
	#output$ModelPredictionsCompareTable <- renderDataTable(modelComparision)
	output$ModelPredictionsCompareTable <- renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  
	  if (is.null(data.sets))
	  {
	    return (NULL)
	  }
	  
	  compareModels(input$model1Select, input$model2Select, 0.02)
	})
	
	output$data_cpu_time <- renderPlotly({
	  arModelBaseChanged()
	  windowsChanged()
	  enabledModelsChanged()
	  hiddenLayersChanged()
	  excludeBiasChanged()
	  arModelBaseChanged()
	  
	  id <- input$idSelect
	  
	  if (input$cpuTimeOfAllTimeseries)
	  {
	    p <- plot_ly(type = 'box')
	    
	    for (modelName in vars$enabledModels)
	    {
	      p <- p %>% add_boxplot(y = getCpuTimes(modelName, na.rm = input$excludeNAModels),
	        line = list(color = modelColors[[modelName]]), name = modelName, boxmean = TRUE)
	    }
	  }
	  else
	  {
	    x <- vars$enabledModels
	    cpu_times <- unlist(lapply(vars$enabledModels, function(modelName) {
	      getCpuTimes(modelName, input$idSelect, na.rm = input$excludeNAModels)
	    }))
	    
	    #sort x
	    x <- factor(x, levels = unique(x)[order(cpu_times, decreasing = TRUE)])
	    
	    p <- plot_ly(
	      x = x, #c("one modell", "one modell hidden","AR"),# "all time series without hidden","AR"),# "all time series with hidden"),
	      y = cpu_times,
	      type = "bar"
	    )
	  }
	  
	  p$elementId <- NULL
	  p
	})
}
