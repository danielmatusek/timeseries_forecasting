library(data.table)
if (!require("DT")) install.packages('DT')
library(forecast)
library(hydroGOF) #rmse function
library(neuralnet)
library(stats)
library(TSPred) #sMape function
library(zoo)

source('ar.r')
source('elman.r')
source('jordan.r')
source('mlp.r')
source('comparision.r')
source('plot.rsnns.r')
source('keras.r')

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB

server <- function(input, output, session) {
  values <- reactiveValues(dataImported = 0)
  
  ###
  ### Load Data Source
  ###
  
  observeEvent(input$importDataModal, {
    dataSources <- getAvailableDataSources()
    
    showModal(modalDialog(
      selectInput('dataInput', 'Use Dataset:', dataSources),
      conditionalPanel(
        condition = 'input.dataInput.substr(-4).toLowerCase() == ".csv"',
        checkboxInput('dataSourceContainsHeader', 'Contains Header', TRUE),
        radioButtons('dataSourceSeparator', 'Separator',
          c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ',')
      ),
      footer = tagList(
        actionButton('importData', 'Import Data'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$importData, {
    removeModal()
    
    path <- paste0('../resources/', input$dataInput)
    length <- nchar(path)
    
    # Save data in a temporary variable
    if (tolower(substr(path, length - 3, length)) == '.csv')
    {
      values$rawData <- read.csv(path, header = input$dataSourceContainsHeader, sep = input$dataSourceSeparator)
    }
    else
    {
      values$rawData <- readRDS(path)
    }
    columns <- names(values$rawData)
    
    showModal(modalDialog(
      selectInput('idColumn', 'ID Name', columns, selected = columns[1]),
      selectInput('xColumn', 'X-Axis', columns, selected = columns[2]),
      selectInput('yColumn', 'Y-Axis', columns, selected = columns[3]),
      footer = tagList(
        actionButton('setDataStructure', 'OK'),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$setDataStructure, {
    removeModal()
    
    parseData(values$rawData, idName = input$idColumn, xName = input$xColumn, yName = input$yColumn)
    resetModels(availableModels)
    
    values$rawData <- NULL  # Remove temporarily stored data
    values$dataImported <- values$dataImported + 1
  })
  
  
  
  ###
  ### Save/Load Results
  ###
  
  observeEvent(input$openLoadResultsModal, {
    results <- getAvailableResuls()
    
    showModal(
      if (length(results) > 0)
      {
        modalDialog(
          selectInput('savedResults', 'Load Results', getAvailableResuls()),
          footer = tagList(
            actionButton('loadResults', 'Load'),
            modalButton('Cancel')
          ))
      }
      else
      {
        modalDialog(
          div('There are no results to load.'),
          footer = tagList(
            modalButton('OK')
          ))
      }
    )
  })
  
  showSaveModal <- function(failed = FALSE)
  {
    showModal(modalDialog(
      textInput('resultName', 'Result Name'),
      if (failed)
      {
        div(tags$b('Name must not be empty', style = "color: red;"))
      },
      footer = tagList(
        actionButton('saveResults', 'Save'),
        modalButton('Cancel')
      )
    ))
  }
  
  observeEvent(input$openSaveResultsModal, {
    showSaveModal()
  })
  
  observeEvent(input$loadResults, {
    removeModal()
    loadResults(input$savedResults)
    
    values$dataImported <- values$dataImported + 1
    
    options <- vars$options
    updateSliderInput(session, 'windowSize', value = options$windowSize,
      min = options$windowSize, max = options$windowSize)
    updateSliderInput(session, 'horizon', value = options$horizon,
      min = options$horizon, max = options$horizon)
    updateRadioButtons(session, 'arModelName', selected = vars$options$arModelName)
    updateCheckboxInput(session, 'excludeBias', value = vars$options$excludeBias)
    updateCheckboxGroupInput(session, 'enabledModels', selected = vars$enabledModels)
    updateSliderInput(session, 'hiddenNeuronsInFirstLayer', value = options$hiddenLayers[1],
      min = options$hiddenLayers[1], max = options$hiddenLayers[1])
  })
  
  observeEvent(input$saveResults, {
    resultName <- input$resultName
    
    if (resultName != '')
    {
      saveResults(resultName)
      removeModal()
    }
    else
    {
      showSaveModal(failed = TRUE)
    }
  })
	
	
  
  ###
	### Settings Changed Events
  ###
  
  windowsChanged <- reactive({
    if (input$windowSize != vars$options$windowSize || input$horizon != vars$options$horizon)
    {
      vars$options$windowSize <<- input$windowSize
      vars$options$horizon <<- input$horizon
      
      resetModels(availableModels)
      resetWindows()
      resetComparison()
      resetNeuralNetworks.InputExclusion()
    }
  })

	excludeBiasChanged <- reactive({
	  if (input$excludeBias != vars$options$excludeBias)
	  {
	    vars$options$excludeBias <<- input$excludeBias
	    
	    resetModels('ar')
	    resetNeuralNetworks()
	    resetNeuralNetworks.InputExclusion()
	  }
	})
	
	hiddenLayersChanged <- reactive({
	  hiddenLayers <- c(input$hiddenNeuronsInFirstLayer)
	  if (hiddenLayers != vars$options$hiddenLayers)
	  {
	    vars$options$hiddenLayers <<- hiddenLayers
	    
	    resetNeuralNetworks.hidden()
	    resetNeuralNetworks.InputExclusion()
	    resetModels('mlp', 'lstm')
	  }
	})
	
	excludeInputErrorChanged <- reactive({
	  if (neuralnetwork.greedyErrorType != input$inputSelectedErrorType)
	  {
	    neuralnetwork.greedyErrorType <<- input$inputSelectedErrorType
	    resetNeuralNetworks()
	    resetNeuralNetworks.InputExclusion()
	  }
	})
	
	enabledModelsChanged <- reactive({
	  vars$enabledModels <<- input$enabledModels
		resetComparison()
	})
	
	arModelBaseChanged <- reactive({
	  if (input$arModelName != vars$options$arModelName)
	  {
	    vars$options$arModelName <<- input$arModelName
	    
	    resetModels('ar')
	  }
	})
	
	
	
	### UI elements: General
	
	output$idSelectBox <- renderUI({
	  if (values$dataImported > 0)
	  {
	    selectInput("idSelect", "Dataset", sort(as.numeric(names(vars$timeSeries))))
	  }
	})
	
	output$windowSize <- renderUI({
	  values$dataImported
	  id <- input$idSelect
	  
	  if (is.null(vars$timeSeries) || is.null(id))
	  {
	    sliderInput('windowSize', 'Window Size', 0, 0, 0, step = 1)
	  }
	  else
	  {
	    numData <- length(vars$timeSeries[[id]]$x)
	    values <- input$windowSize
	    if(is.null(values)){
	      values <- 0.0175*numData
	    }
	    sliderInput('windowSize', 'Window Size', 1, 30, values, step = 1)
	  }
	})
	
	output$horizon <- renderUI({
	  windowSize <- input$windowSize
	  
	  if(is.null(windowSize) || windowSize == 0)
	  {
	    sliderInput('horizon', 'Predict Values', 0, 0, 0, step = 1)
	  }
	  else
	  {
	    values <- input$horizon
	    if(is.null(values)){
	      values <- windowSize
	    }
	    sliderInput('horizon', 'Predict Values', 1, 20 , values, step = 1)
	  }
	})
	
	output$hiddenSliderInput <- renderUI({
	  windowSize <- input$windowSize
	  
	  if (is.null(windowSize) || windowSize == 0)
	  {
	    sliderInput('hiddenNeuronsInFirstLayer', "Number Hidden Neurons", 0, 0, 0, step = 1)
	  }
	  else
	  {
	    values <- input$hiddenNeuronsInFirstLayer
	    if(is.null(values)){
	      values <- 3
	    }
	    maxHiddenSlider <- input$windowSize * 2
	    sliderInput('hiddenNeuronsInFirstLayer', "Number Hidden Neurons", 1, maxHiddenSlider, values, step = 1)
	  }
	})
	
	
	
	### UI elements: Data
	
	
	output$dataChart <- renderPlotly({
	  values$dataImported
	  
		p <- plot_ly(vars$timeSeries[[input$idSelect]], x = ~x, y = ~y, type = 'scatter', mode = 'lines')
		p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
		p
	})
	
	output$dataTable <- renderDataTable({
	  values$dataImported
	  
	  vars$timeSeries[[input$idSelect]]
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
	  
	  supportedNeuralNetworks <- c('nnfe', 'nnfeh', 'nnfa', 'nnfah', 'nnfeei','nnfehei','nnfed','nnfehd','nnfamei')
	  optimizedNeuralNetworks <- c('nnfeei','nnfehei', 'nnfamei')
	  enabledNeuralNetworks <- intersect(vars$enabledModels, supportedNeuralNetworks)
	  
	  tabs <- lapply(enabledNeuralNetworks, function(nnName) {
	    if (nnName %in% optimizedNeuralNetworks)
	    {
	      tabPanel(nnName,
	               plotOutput(paste0(nnName, 'Plot'), height = '600px'),
	               dataTableOutput(paste0(nnName, 'OptimizingTable')),
	               dataTableOutput(paste0(nnName, 'OptimizingTable2'))
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
	
	
	
	# Plot Exclude Inputs 
	
	output$nnfeeiPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()

	  plot(getModel('nnfeei', input$idSelect), rep = 'best')
	})
	
	
	
	# Plot Exluded Input Stats
	
	output$nnfameiPlot <- renderPlot({
	  windowsChanged()
	  excludeBiasChanged()
	  hiddenLayersChanged()
	  excludeInputErrorChanged()
      
	  m <- getModel('nnfamei', NULL)
	  #browser()
	  dat <- as.data.frame( m$numOfExclusionPerNode, m$inputNodePos )
	  barplot(m$numOfExclusionPerNode, xlab = "Input node position", ylab = "Number", col = c("darkblue"), names.arg=m$inputNodePos)#, horz= TRUE)
	})
	

	
	# Exclude Inputs Datatable
	
	
	getExcludedInputTable <- function(modelName)
	{
	  s <- getModel(modelName, input$idSelect)
	  path <- s$path
	  pathes <- names(s$inSampleError)
	  l <- length(s$inSampleError)
	  info <- rep(0, l)
	  
	  if(is.null(path))
	  {
	    info[1] <- 2
	  }
	  else
	  {
	    info[1] <- 1
	    counter <- 1
	    for(i in 2 : l)
	    {
	      subPath <- paste(path[1 : counter], collapse = ",")
	      if(subPath %in% pathes[i])
	      {
	        if(counter == length(path))
	        {
	          info[i] <- 2
	          break
	        }
	        else
	        {
	          info[i] <- 1
	          counter <- counter + 1 
	        }
	      }
	    }
	  }
	  
	  dt <- data.table("Excluded Nodes" = names(s$inSampleError), "sMAPE" = s$outSampleError, "Sampling Error" = s$inSampleError, "info" = info)
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
	  excludeInputErrorChanged()
	  
	  getExcludedInputTable('nnfeei')
	})
	

	
	# Data table for excluded Input stats
	output$nnfameiOptimizingTable <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfamei')
	  data.table('Input Node' = c(m$inputNodePos), 'Number of exclusion' = c(m$numOfExclusionPerNode))
	})
	
	output$nnfameiOptimizingTable2 <- DT::renderDataTable({
	  windowsChanged()
	  excludeBiasChanged()
	  excludeInputErrorChanged()
	  
	  m <- getModel('nnfamei')
	  data.table( 'Path' = c(m$excludedInputs), 'Ids' = c(m$ids))
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
		vars$options$hiddenLayers <<- neuralNetwork.tempHiddenNodes
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
  
  	plot(getModel('elman', input$idSelect), paste0('xt', 1:vars$options$windowSize))
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
  
  	plot(getModel('mlp', input$idSelect), paste0('xt', 1:vars$options$windowSize))
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
  
  	plot(getModel('mlph', input$idSelect), paste0('xt', 1:vars$options$windowSize))
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

		plot(getModel('jordan', input$idSelect), paste0('xt', 1:vars$options$windowSize))
	})
	
	
	
	
	
	### UI elements: Auto Regression
	
	
	output$arACF <- renderPlot({
	  values$dataImported
	  
	  acf(vars$timeSeries[[input$idSelect]]$y, main = "ACF")
	})
	
	output$arPACF <- renderPlot({
	  values$dataImported
	  
	  pacf(vars$timeSeries[[input$idSelect]]$y, main = "PACF")
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
	  
	  getErrorMetricCompare(input$idSelect)
	})
	
	output$compareCoefficient <- renderDataTable({
	  values$dataImported
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
	  
	  if (is.null(vars$timeSeries))
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
	  
	  if (is.null(vars$timeSeries))
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
