library(plotly)
library(shinydashboard)

ui <- dashboardPage(skin = 'purple',
	dashboardHeader(title = 'FPADB',
	  tags$li(a(
	    conditionalPanel(
	      condition = '$("html").hasClass("shiny-busy")',
	      icon('circle-o-notch', 'fa-spin'),
	      tags$span('loading...'))
	    ),
	    class = 'dropdown'
	)),

	dashboardSidebar(
	  conditionalPanel('output.idColumnSelect != null',# cannot depend on input$dataFile directly.
	    sidebarMenu(id="tabs",
	      menuItem('Settings', tabName = 'settings', icon = icon('cogs')),
	              menuItem("Data", tabName = "data", icon = icon("database")),
		            menuItem("Neural Network", tabName = "neuralNetwork", icon = icon("sitemap", "fa-rotate-90")),
		            menuItem("Autoregressive", tabName = "aRModel", icon = icon("line-chart")),
		            menuItem("Comparision", tabName = "comparision", icon = icon("balance-scale")),
	      hr(),
		            uiOutput("idSelectBox")
		  )
	  )
	),

	dashboardBody(
		tabItems(
		  tabItem(tabName = 'settings',
		    fluidRow(style = 'background-color: #fff; margin: 0;',
		      column(3,
		        h3('Data Input', style = 'margin-bottom: 20px;'),
		        checkboxInput('headerCheckbox', 'Header', TRUE),
		        radioButtons('separatorRadioButton', 'Separator',
		          c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),	
		        fileInput('dataFile', NULL,
		          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
		        ),
		        uiOutput('idColumnSelect'),
		        uiOutput("x_axis"),
		        uiOutput("y_axis")
		      ),
		      column(3,
		        h3('General', style = 'margin-bottom: 20px;'),
		        uiOutput('windowSizeSlider'),
		        uiOutput('horizonSlider')
		      ),
		      column(3,
		        h3('Neural Network', style = 'margin-bottom: 20px;'),
		        checkboxInput('biasCheckbox', 'Exclude Bias', TRUE),
		        radioButtons('normalizationRadioButton', 'Normalization',
		          c('None' = 'none', 'Z-Normalization' = 'zScore', 'Min-Max Scale' = 'minmax'), 'minmax'),
		        uiOutput('hiddenSliderInput')
		      ),
		      column(3,
		        h3('Autoregression', style = 'margin-bottom: 20px;'),
		        radioButtons('aRModelName', 'Arima Models',
		          c(AR='AR', AutoArima='AutoArima'), 'AR')
		      )
		    )
		  ),
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
			          tabPanel('Result',
			            plotlyOutput('forecastComparisionPlot', height = '600px')
			         ),
			               tabPanel("MSE",
			                        plotlyOutput("compareMSE", height = "600px")
			               ),
			               tabPanel("RMSE",
			                        plotlyOutput("compareRMSE", height = "600px")
			               ),
			               tabPanel("SMAPE",
			                        plotlyOutput("compareSMAPE", height = "600px")
			               ),
			               tabPanel("Average Error",
			                        dataTableOutput("compareError")
			               ),
			               tabPanel("Coefficients",
			                        dataTableOutput("compareCoefficient")
			               ),
			               tabPanel("Forecast",
			                        plotlyOutput("compareForecast", height = "600px")
			               )
			        )
			)
		)
	)
)
