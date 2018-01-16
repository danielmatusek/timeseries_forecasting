library(plotly)
library(shinydashboard)

source('model.r')

ui <- dashboardPage(skin = 'purple',
	dashboardHeader(title = 'FPADB',
	  tags$li(a(
	    conditionalPanel(
	      condition = '$("html").hasClass("shiny-busy")',
	      icon('circle-o-notch', 'fa-spin'),
	      tags$span('loading...'))
	    ),
	    class = 'dropdown'
	  ),
	  tags$li(actionLink('openSaveResultsModal', label = '', icon = icon('save')), class = 'dropdown'),
	  tags$li(actionLink('openLoadResultsModal', label = '', icon = icon('folder-open')), class = 'dropdown')
	),

	dashboardSidebar(
	  conditionalPanel('output.idColumnSelect != null',# cannot depend on input$dataFile directly.
	    sidebarMenu(id="tabs",
	      menuItem('Settings', tabName = 'settings', icon = icon('cogs')),
	              menuItem("Data", tabName = "data", icon = icon("database")),
		            menuItem("Neural Network", tabName = "neuralNetwork", icon = icon("sitemap", "fa-rotate-90")),
	              menuItem("RSNNS Package", tabName = "rsnnspackage", icon = icon("sitemap", "fa-rotate-90")),
		            menuItem("Autoregressive", tabName = "aRModel", icon = icon("line-chart")),
		            menuItem("Comparision", tabName = "comparision", icon = icon("balance-scale")),
	              menuItem("Hidden Nodes", tabName = "hlOptimization", icon = icon("stethoscope")),
	      hr(),
		            uiOutput("idSelectBox")
		  )
	  )
	),

	dashboardBody(
		tabItems(
		  tabItem(tabName = 'settings',
		    fluidRow(style = 'background-color: #fff; margin: 0;',
		      column(4,
		        h3('Data Input', style = 'margin-bottom: 20px;'),
		        checkboxInput('headerCheckbox', 'Header', TRUE),
		        radioButtons('separatorRadioButton', 'Separator',
		          c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),	
		        fileInput('dataFile', NULL,
		          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
		        ),
		        selectInput("use_data","Use Dataset:",c("load csv"="csv","alipay" = "alipay","metadata_complete"="metadata")),
		        uiOutput('idColumnSelect'),
		        uiOutput("x_axis"),
		        uiOutput("y_axis")
		      ),
		      column(4,
		        h3('General', style = 'margin-bottom: 20px;'),
		        uiOutput('windowSize'),
		        uiOutput('horizon'),
		        checkboxGroupInput('enabledModels', 'Enable Models', availableModels, selected = vars$enabledModels)
		      ),
		      column(4,
		        h3('Autoregression', style = 'margin-bottom: 20px;'),
		        radioButtons('arModelName', 'Arima Models',
		          c(AR='ar', AutoArima='autoArima'), 'ar'),
		        h3('Neural Network', style = 'margin: 40px 0 10px 0;'),
		        checkboxInput('excludeBias', 'Exclude Bias', TRUE),
		        checkboxInput('inputDifferenceCheckbox', 'Use Difference', FALSE),
		        checkboxInput('inputCheckbox', 'Exclude Input', FALSE),
		        uiOutput('inputStrategy'),
		        uiOutput('inputSelectedErrorType'),
		        uiOutput('hiddenSliderInput'),
						checkboxGroupInput("variable_nn_hidden", "neural network hidden nodes optimization",
																c("Optimize" = "optimize_hidden_layer"),
																selected= c("optimize_hidden_layer"))
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
			        uiOutput("neuralNetwork_tabs")
			),
			
			
			tabItem(tabName = "rsnnspackage",
				tabBox(width = NULL,
					tabPanel("RNN/Elman",
			        dataTableOutput("reccurentNeuralNetwork_tab"),
							plotOutput('recPlot', height = '600px')
					),
					tabPanel("MLP",
							dataTableOutput("rsnns_mlp_tab"),
							plotOutput('mlp_plot')
					),
					tabPanel("MLP wit Hidden Layer",
							dataTableOutput("rsnns_mlp_tab_without_hidden"),
							plotOutput('mlp_plot_without_hidden')
					),
					tabPanel("RNN/Jordan",
							dataTableOutput("rsnns_jordan_tab"),
							plotOutput('rsnns_jordan_plot')
					)
				)
			),
			
			
			tabItem(tabName = "aRModel",
			        tabBox(width = NULL,
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
			               tabPanel('Error',
			                 fluidRow(
			                   column(width = 6,
			                     selectInput('errorMetricName', 'Error metric', c('smape', 'mse', 'rmse'), selected = 'smape')
			                   ),
			                   column(width = 6,
			                     checkboxInput('errorOfAllTimeseries', 'Display error of all time series')
			                   )
			                 ),
			                 plotlyOutput('errorMetricPlot', height = '600px')
			               ),
			               tabPanel("Mean Error",
			                        dataTableOutput("compareError")
			               ),
			          
			               tabPanel("Coefficients",
			                        dataTableOutput("compareCoefficient")
			               ),
			          #tabPanel('NN Dif. wrt HL',
			          #  dataTableOutput('neuralNetworkDifferenceWRTHiddenLayers')
			          #),
			          tabPanel('Model Predictions',
			            fluidRow(
			              column(width = 5,
			                selectInput('model1Select', 'Model 1', availableModels, selected = 'ar')
			              ),
			              column(width = 5,
			                selectInput('model2Select', 'Model 2', availableModels, selected = 'nnfe')
			              ),
			              column(width = 2,
			                actionButton('compareModels', 'Vergleichen')
			              )
			            ),
			            dataTableOutput('ModelPredictionsCompareTable')
			          ),
			         tabPanel('CPU time',
			           fluidRow(
			             column(width = 6,
			               checkboxInput('cpuTimeOfAllTimeseries', 'CPU time of all time series')
			             ),
			             column(width = 6,
			               checkboxInput('excludeNAModels', 'Exclude non-converging models', TRUE)
			             )
			           ),
			           plotlyOutput('data_cpu_time')
			         )
			        )
			),
			tabItem(tabName="hlOptimization",
			        uiOutput("neuralNetwork_hlOptimization")
			)
		)
	)
)
