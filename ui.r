library(plotly)
library(shinydashboard)

ui <- dashboardPage( skin = "purple",
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
		                             uiOutput("hiddenSliderInput"),
		                             hr()),
		            uiOutput("idSelectBox"),
		            uiOutput('normalizationRadioButton'),
		    		    uiOutput('windowSizeSlider'),
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
					#tabPanel("Chart",
					#	plotOutput("neuralNetworkChart", height = "600px")
					#),
				  tabPanel('Forecast /1',
				    plotOutput("neuralNetworkChart", height = "600px"),
				    plotlyOutput('neuralNetworkForecastForEachChart')
				  ),
				  tabPanel('Forecast /1 hidden',
				    plotOutput("neuralNetworkHiddenChart", height = "600px"),       
				    plotlyOutput('neuralNetworkForecastForEachHiddenChart')
				  ),
				  tabPanel('Forecast /n',
				    plotOutput("neuralNetworkChartForAll", height = "600px"), 
				    plotlyOutput('neuralNetworkForecastForAllChart')
				  ),
				  tabPanel('Forecast /n hidden',
				    plotOutput("neuralNetworkHiddenChartForALL", height = "600px"), 
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
			               ),
			               tabPanel("Time Series Analysis",
			                        plotOutput("arACF"),
			                        plotOutput("arPACF")
			               )
			        )
			),
			tabItem(tabName = "comparision",
			        tabBox(width = NULL,
			               tabPanel("MSE",
			                        plotlyOutput("compareMSE", height = "600px")
			               ),
			               tabPanel("RMSE",
			                        plotlyOutput("compareRMSE", height = "600px")
			               ),
			               tabPanel("SMAPE",
			                        plotlyOutput("compareSMAPE", height = "600px")
			               ),
			               tabPanel("Mean Error",
			                        dataTableOutput("compareError")
			               )
			        )
			)
			
			
			
		)
	)
)
