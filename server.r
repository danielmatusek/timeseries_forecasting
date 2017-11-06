library(shiny)
library(shinydashboard)
library(plotly)
library(neuralnet)
library(zoo)


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

		hr(),

		sidebarMenu(id="tabs",
			menuItem("Data", tabName = "data", icon = icon("database")),
			menuItem("Neural Network", tabName = "neuralNetwork", icon = icon("sitemap", "fa-rotate-90"))
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
					)
				)
			)
		)
	)
)

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
	  
	  index <- 1:nrow(ws)
	  trainindex <- sample(index, trunc(length(index) * dataSplitFactor / 100))
	  trainset <- ws[trainindex, ]
	  testset <- ws[-trainindex, ]
	  
	  list(trainset = trainset, testset = testset)
	})

	neuralNetwork <- reactive({
		nnData = windowSplit()

		if (is.null(nnData))
		{
			return(NULL)
		}
		
		nnData <- nnData$trainset

		n <- names(nnData)
		f <- as.formula(paste("xt0 ~ ", paste(n[!n %in% "xt0"], collapse = " + ")))

		neuralnet(f, nnData, hidden = 4, rep = 10, linear.output = FALSE)
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
	  
	  expected <- windows$trainset$xt0
	  windows$trainset$xt0 <- NULL
	  
	  n <- compute(network, windows$trainset)
	  n$net.expected <- expected
	  
	  
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
	  
	  n$net.result <- n$net.result * scale + offset
	  n$net.expected <- n$net.expected * scale + offset
	  
	  n
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
}

shinyApp(ui, server)
