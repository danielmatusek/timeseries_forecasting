library(plotly)
library(shinydashboard)

ui <- dashboardPage(
	dashboardHeader(title = "FPADB"),

	dashboardSidebar(
	  sidebarMenu(id="tabs",
	              menuItem("Data", tabName = "data", icon = icon("database")),
		            menuItem("Neural Network", tabName = "neuralNetwork", icon = icon("sitemap", "fa-rotate-90")),
		            menuItem("Autoregressive", tabName = "aRModel", icon = icon("table")),
		            menuItem("Comparision", tabName = "comparision", icon = icon("table")),
		            hr(),
                
		            conditionalPanel("input.tabs === 'data'",
		                             checkboxInput('headerCheckbox', 'Header', TRUE),
		                             radioButtons('separatorRadioButton', 'Separator',
		                                          c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),
		                             uiOutput("x_axis"),
		                             uiOutput("y_axis"),
		                             uiOutput("z_axis"),		
		                             fileInput('dataFile', NULL,
		                                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
		                             ),
		                             hr()
		            ),
		            conditionalPanel("input.tabs === 'neuralNetwork'",
		                             checkboxInput('biasCheckbox', 'Exclude Bias in ANN', TRUE),
		                             hr()),
		            uiOutput("idSelectBox"),
		            uiOutput('normalizationRadioButton'),
		    		    uiOutput('windowSizeSlider'),
		            sliderInput('dataSplitSlider', 'Split Training/Test Data', 1, 100, 70, post = " %", step = 1),
		    		    uiOutput('horizonSlider')
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
				  tabPanel('Forecast /1',
				    plotlyOutput('neuralNetworkForecastForEachChart')
				  ),
				  tabPanel('Forecast /1 hidden',
				    plotlyOutput('neuralNetworkForecastForEachHiddenChart')
				  ),
				  tabPanel('Forecast /n',
				    plotlyOutput('neuralNetworkForecastForAllChart')
				  ),
				  tabPanel('Forecast /n hidden',
				    plotlyOutput('neuralNetworkForecastForAllHiddenChart')
				  )
				)
			),
			
			tabItem(tabName = "aRModel",
			        tabBox(width = NULL,
			               tabPanel("Chart",
			                        plotlyOutput("aRChart", height = "600px")
			               ),
			               tabPanel("Statistic",
			                        dataTableOutput("arMLE"),
			                        dataTableOutput("arCoef")
			               )
			        )
			)
		)
	)
)
