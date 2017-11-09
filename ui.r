library(plotly)
library(shinydashboard)

ui <- dashboardPage(
	dashboardHeader(title = "FPADB"),

	dashboardSidebar(
		checkboxInput('headerCheckbox', 'Header', TRUE),
		radioButtons('separatorRadioButton', 'Separator',
			c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),
		uiOutput("x_axis"),
		uiOutput("y_axis"),
		uiOutput("z_axis"),		
		fileInput('dataFile', NULL,
			accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
		),

		hr(),

		uiOutput("meteridSelectBox"),
		radioButtons('normalizationRadioButton', 'Normalization',
			c('None' = 'none', 'Z-Normalization' = 'zScore', 'Min-Max Scale' = 'minmax'), 'none'),
		uiOutput('windowSizeSlider'),
		sliderInput('dataSplitSlider', 'Split Training/Test Data', 1, 100, 70, post = " %", step = 1),
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
			               tabPanel("Statistic",
			                        plotlyOutput("aRStatistic", height = "600px")
			               )
			        )
			)
		)
	)
)
