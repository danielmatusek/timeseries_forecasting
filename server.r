library(shiny)
library(shinydashboard)
library(plotly)
library(neuralnet)

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
						plotlyOutput("dataChart", height="500px")
					),
					tabPanel("Table",
						dataTableOutput("table")
					)
				)
			),

			tabItem(tabName = "neuralNetwork",
				tabBox(width = NULL,
					tabPanel("Chart",
						plotOutput("neuralNetworkChart", height="500px")
					),
					tabPanel("Tabelle",
						dataTableOutput("neuralNetworkTable")
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

	neuralNetworkData <- reactive({
		df <- dataset()
		numInputNeurons <- 5

		if (is.null(df))
		{
			return(NULL)
		}

		numCols = numInputNeurons + 1
		numRows = length(df$consumption) - numInputNeurons

		d <- data.frame(xt = df$consumption[numCols:400])
		for (i in 1:numInputNeurons)
		{
			d[paste('xt', i, sep = '')] <- df$consumption[(numCols - i):(length(df$consumption) - i)]
		}
		d
	})

	neuralNetwork <- reactive({
		nnData = neuralNetworkData()

		if (is.null(nnData))
		{
			return(NULL)
		}

		n <- names(nnData)
		f <- as.formula(paste("xt ~ ", paste(n[!n %in% "xt"], collapse = " + ")))

		neuralnet(f, nnData, hidden=0, rep = 10, linear.output=FALSE)
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

	output$neuralNetworkChart <- renderPlot({
		nn <- neuralNetwork()

		if (is.null(nn))
		{
			return(NULL)
		}

		plot(nn, rep = "best")
	})
}

shinyApp(ui, server)
