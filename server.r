library(data.table)
if (!require("DT")) install.packages('DT')
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
	  resetModels('ar')
	  resetComparison()
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	})
	
	
	idChanged <- reactive({
	  data.idSelected <<- input$idSelect
	  resetNeuralNetworks.InputExclusion()
	})
	
	inputDifferenceChanged <- reactive({
	  resetWindows()
	  data.windowSize <<- input$windowSizeSlider
	  neuralNetwork.inputDifference <<- input$inputDifferenceCheckbox
	  resetWindows()
	  resetModels('ar')
	  resetComparison()
	  resetNeuralNetworks()
	  resetNeuralNetworks.InputExclusion()
	  resetNeuralNetworks()
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
	   if(input$inputCheckbox == TRUE && input$windowSizeSlider > 1)
	  {
	    selectInput("inputSelectedErrorType", "Error Type", c("Outsample", "Insample"))
	    
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
	  enabledModelsChanged()
	  excludeInputChanged()
	  
	  supportedNeuralNetworks <- c('nnfe', 'nnfeh', 'nnfa', 'nnfah')
	  enabledNeuralNetworks <- intersect(vars$enabledModels, supportedNeuralNetworks)
	  
	  tabs <- lapply(enabledNeuralNetworks, function(nnName) {
	    tabPanel(nnName, plotOutput(paste0(nnName, 'Plot'), height = '600px'))
	  })
	 	
	 	if(neuralNetwork.isInputExcluded)
	 	{
	 	  tabsInputExcluded <- lapply(enabledNeuralNetworks, function(nnName) {
	 	    tabPanel(paste(nnName, 'Ex. Inp.'), plotOutput(paste0(nnName, 'InputExcludedPlot'), height = '600px'),
	 	      dataTableOutput(paste0(nnName, 'InputExcludedTable')))
	 	  })
	 	  
	 	  tabs <- append(tabs, tabsInputExcluded)
	 	}
	 	
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
	  idChanged()
	  inputDifferenceChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  
		plot(getModel('nnfe', input$idSelect), rep = 'best')
	})
	
	output$nnfehPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  
	  plot(getModel('nnfeh', input$idSelect), rep = 'best')
	})
	
	output$nnfaPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  
	  plot(getModel('nnfa'), rep = 'best')
	})
	
	output$nnfahPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  inputDifferenceChanged()
	  hiddenLayersChanged()
	  
	  plot(getModel('nnfah'), rep = 'best')
	})
	
	# Exclude Inputs Plot
	
	output$nnfeInputExcludedPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputDifferenceChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  plot(getExcludedInputNeuralNetwork(input$idSelect, hiddenLayers = FALSE, input$inputStrategy), rep = 'best')
	})
	
	output$nnfehInputExcludedPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  plot(getExcludedInputNeuralNetwork(input$idSelect, TRUE, input$inputStrategy), rep = 'best')
	})
	
	output$nnfaInputExcludedPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputDifferenceChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  plot(getExcludedInputNeuralNetwork(NULL, FALSE, input$inputStrategy), rep = 'best')
	})
	
	output$nnfahInputExcludedPlot <- renderPlot({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  plot(getExcludedInputNeuralNetwork(NULL, TRUE, input$inputStrategy), rep = 'best')
	})
	
	
	# Exclude Inputs Data
	
	getExcludedInputTable <- function(number)
	{
	  s <- neuralNetwork.excluded.statistics[[number]]
	  dt <- data.table("Excluded Nodes" = s$nodes, "sMAPE" = s$smape, "Sampling Error" = s$internalE, taken = s$pathAsIndices)
	  dt <- dt[rowSums(is.na(dt)) == 0,]
	  datatable(head(dt, 50),
	            class = 'cell-border stripe',
	            options = list(
	              columnDefs = list(list(targets = 4, visible = FALSE)),
	              pageLength = 50))%>%
	    formatStyle("taken",target = 'row',color = "black", backgroundColor = styleEqual(c(0, 1,2), c('gray', 'yellow','green')), fontWeight = styleEqual(c(2), c('bold')))
	  
	}
	

	output$nnfeInputExcludedTable <- DT::renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputDifferenceChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable(2)
	})
	

	output$nnfehInputExcludedTable <- DT::renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputDifferenceChanged()
	  inputStrategyChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable(4)
	})
	
	output$nnfaInputExcludedTable <- DT::renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable(1)
	})
	
	output$nnfahInputExcludedTable <- DT::renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeInputChanged()
	  inputStrategyChanged()
	  inputDifferenceChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable(3)
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
		
		t <- getTestResults('elman', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	#MLP with RSNNS and with Hidden Layer

	output$rsnns_mlp_tab <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlp', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	#MLP with RSNSS and without Hidden Layer
	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlp', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$recPlot <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('elman', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlph', input$idSelect)
		
		data.table(result = t$result, expected = t$expected)
	})

	output$mlp_plot <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('mlp', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_mlp_tab_without_hidden <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('mlph', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$mlp_plot_without_hidden <- renderPlot({
  	idChanged()
  	windowsChanged()
  	excludeInputChanged()
  	excludeBiasChanged()
  	hiddenLayersChanged()
  
  	plot(getModel('mlph', input$idSelect), paste0('xt', 1:data.windowSize))
	})

	output$rsnns_jordan_tab <- renderDataTable({
		idChanged()
		windowsChanged()
		excludeInputChanged()
		excludeBiasChanged()
		hiddenLayersChanged()
		
		t <- getTestResults('jordan', input$idSelect)
		
		data.table(predicted = t$predicted, expected = t$expected)
	})

	output$rsnns_jordan_plot <- renderPlot({
		idChanged()
  	windowsChanged()
  	excludeInputChanged()
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
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  inputDifferenceChanged()
	  
	  getForecastComparisionPlot(input$idSelect)
	})
	
	output$compareMSE <- renderPlotly({
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  inputDifferenceChanged()
	  
	  getBoxplot('mse')
	})
	
	output$compareRMSE <- renderPlotly({
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  inputDifferenceChanged()
	  
	  getBoxplot('rmse')
	})
	
	output$compareSMAPE <- renderPlotly({
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  errorTypChanged()
	  inputDifferenceChanged()
	  
	  getBoxplot('smape')
	})
	
	output$compareError <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  inputDifferenceChanged()
	  
	  getErrorMetricCompare()
	})
	
	output$compareCoefficient <- renderDataTable({
	  idChanged()
	  databaseChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  enabledModelsChanged()
	  arModelBaseChanged()
	  inputDifferenceChanged()
	  
	  getCoef(input$idSelect)
	})
	
	output$neuralNetworkDifferenceWRTHiddenLayers <- renderDataTable({
	  idChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  enabledModelsChanged()
	  
	  findDifferenceInNeuralNetworksWrtHiddenLayers()
	})
	
	modelComparision <- eventReactive(input$compareModels, {
	  idChanged()
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
	  idChanged()
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
	  
	  id <- input$idSelect
	  
	  
	  x <- vars$enabledModels
    cpu_times <- unlist(lapply(vars$enabledModels, function(modelName) {
      if (modelName %in% oneForAllModels)
      {
        getModel(modelName) # make sure the model is computed
        vars$cpuTimes[[modelName]]
      }
      else
      {
        getModel(modelName, id) # make sure the model is computed
        vars$cpuTimes[[modelName]][[id]]
      }
    }))
    
    #sort x
    x <- factor(x, levels = unique(x)[order(cpu_times, decreasing = TRUE)])
	  
	  p <- plot_ly(
	    x = x, #c("one modell", "one modell hidden","AR"),# "all time series without hidden","AR"),# "all time series with hidden"),
	    y = cpu_times,
	    type = "bar"
	  )
	  p$elementId <- NULL
	  p
	  })
}
