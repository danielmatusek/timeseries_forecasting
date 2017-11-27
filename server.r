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

server <- function(input, output) {
  
  ### Settings Changed Events
  
  rawData <- reactive({
    file <- input$dataFile
    
    if (is.null(file))
    {
      return(NULL)
    }
    
    #read.table(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
    read.csv(file$datapath, header = input$headerCheckbox, sep = input$separatorRadioButton)
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
	  aRModelName <<- input$aRModelName
    
	  resetWindows()
	  resetARModels()
	  resetComparison()
	  resetNeuralNetworks()
	  setNeuralNetworkExcludeVector()
	})
	
	excludeBiasChanged <- reactive({
	  neuralNetwork.excludeBias <<- input$biasCheckbox

	  resetARModels()
	  resetNeuralNetworks()
	})
	
	hiddenLayersChanged <- reactive({
	  neuralNetwork.hiddenLayers <<- c(input$hiddenSliderInput)
	  
	  resetNeuralNetworks.hidden()
	  setNeuralNetworkExcludeVector()
	})
	
	
	nnTypChanged <- reactive({
	  neuralNetwork.type <<- input$variable_nn
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
	    selectInput("idSelect", "Dataset", names(data.sets))
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
	  panels <- list()
	  myTabs <- lapply(input$variable_nn, function(x){
	    #tabPanel("Tab3", p("C phrase"), br(""), p("I can place a chart here"))
	    if(x == "forecast_one"){
	      panels[[length(panels)+1]] <- tabPanel('Forecast /1', 
	                                             plotOutput("neuralNetworkChart", height = "600px"),
	                                             plotlyOutput('neuralNetworkForecastForEachChart')
	      )
	    }
	    else if(x == "forecast_one_hidden"){
	      panels[[length(panels)+1]] <- tabPanel('Forecast /1 hidden', 
	                                             plotOutput("neuralNetworkHiddenChart", height = "600px"),       
	                                             plotlyOutput('neuralNetworkForecastForEachHiddenChart')
	      )
	    }
	    else if(x == "forecast_all" ){
	      panels[[length(panels)+1]] <- tabPanel('Forecast /n', 
	                                             plotOutput("neuralNetworkChartForAll", height = "600px"), 
	                                             plotlyOutput('neuralNetworkForecastForAllChart')
	      )
	    } 
	    else if(x == "forecast_all_hidden" ){
	      panels[[length(panels)+1]] <- tabPanel('Forecast /n hidden',
	                                             plotOutput("neuralNetworkHiddenChartForALL", height = "600px"), 
	                                             plotlyOutput('neuralNetworkForecastForAllHiddenChart')
	      )
	    } 
	  })
	  myTabs$width = "100%"
	  do.call(tabBox, myTabs)
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
	  windowsChanged()
	  return (getNeuralNetworkPredictionPlotly(input$idSelect))
	})
	
	output$neuralNetworkForecastForEachHiddenChart <- renderPlotly({
	  windowsChanged()
	  return (getNeuralNetworkPredictionPlotly(input$idSelect, hiddenLayers = TRUE))
	})
	
	output$neuralNetworkForecastForAllChart <- renderPlotly({
	  windowsChanged()
	  return (getNeuralNetworkPredictionPlotly(input$idSelect, forAll = TRUE))
	})
	
	output$neuralNetworkForecastForAllHiddenChart <- renderPlotly({
	  windowsChanged()
	  return (getNeuralNetworkPredictionPlotly(input$idSelect, forAll = TRUE, hiddenLayers = FALSE))
	})
	
	

	
	
	### UI elements: Auto Regression
	
	
	output$aRChart <- renderPlotly({
	  windowsChanged()
	  databaseChanged()
	  getPlotlyModel(input$idSelect)
	})
	
	output$arMLE <- renderDataTable({
	  windowsChanged()
	  databaseChanged()
	  error_metric(autoRegressiveModels[[input$idSelect]]$expected, autoRegressiveModels[[input$idSelect]]$result)
	})
	
	output$arCoef <- renderDataTable({
	  windowsChanged()
	  databaseChanged()
	  data.table(coef = autoRegressiveModels[[input$idSelect]]$coef)
	})
	
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
	  nnTypChanged()
	  comparison()
	  getForecastComparisionPlot(input$idSelect)
	})
	
	output$compareMSE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  comparison()
	  getBoxplot('MSE')
	})
	
	output$compareRMSE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  comparison()
	  getBoxplot('RMSE')
	})
	
	output$compareSMAPE <- renderPlotly({
	  windowsChanged()
	  nnTypChanged()
	  comparison()
	  getBoxplot('SMAPE')
	})
	
	output$compareError <- renderDataTable({
	  windowsChanged()
	  nnTypChanged()
	  comparison()
	  error_metric_compare()
	})
	
	output$compareCoefficient <- renderDataTable({
	  databaseChanged()
	  windowsChanged()
	  excludeBiasChanged()
	  nnTypChanged()
	  comparison()
	  getCoef(input$idSelect)
	})
	
	
	
	output$ErrorMetricTable <- renderDataTable({
	  result <- neuralNetworkTest()
	  error_metric(result$net.result[,1], result$net.expected, result$net.mse)
	})
	

	
}
