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
	              menuItem("RSNNS Package", tabName = "rsnnspackage", icon = icon("sitemap", "fa-rotate-90")),
		            menuItem("Autoregressive", tabName = "aRModel", icon = icon("line-chart")),
		            menuItem("Comparision", tabName = "comparision", icon = icon("balance-scale")),
	              menuItem("Hidden Nodes", tabName = "hlOptimization", icon = icon("stethoscope")),
	      hr(),
		            uiOutput("idSelectBox"),
	              checkboxInput('errorTypCheck', 'Error eine Zeitreihe', TRUE)
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
		        selectInput("use_data","Use Dataset:",c("load csv"="csv","alipay" = "alipay","metadata_complete"="metadata")),
		      #  checkboxInput("use_data_alipay", "Use alipay_base Dataset", FALSE),
		      #  checkboxInput("use_data_meterdata", "Use meterdata_complete_series Dataset", FALSE),
		        uiOutput('idColumnSelect'),
		        uiOutput("x_axis"),
		        uiOutput("y_axis")
		      ),
		      column(3,
		        h3('General', style = 'margin-bottom: 20px;'),
		        uiOutput('windowSizeSlider'),
		        uiOutput('horizonSlider'),
		        checkboxGroupInput('enabledModels', 'Enable Models',
		          c('ar' = 'ar',
		            "one Time Series" = "nnfe",
		            "one TS with hidden" = "nnfeh",
		            "all Time Series" = "nnfa",
		            "all TS with hidden" = "nnfah"),
		          selected = c('ar', 'nnfe', 'nnfeh'))
		      ),
		      column(3, 
		        h3('Neural Network', style = 'margin-bottom: 10px;'),
		        checkboxInput('biasCheckbox', 'Exclude Bias', TRUE),
		        checkboxInput('inputDifferenceCheckbox', 'Use Difference', FALSE),
		        checkboxInput('inputCheckbox', 'Exclude Input', FALSE),
		        uiOutput('inputStrategy'),
		        uiOutput('inputSelectedErrorType'),
		        uiOutput('hiddenSliderInput'),
						checkboxGroupInput("variable_nn_hidden", "neural network hidden nodes optimization",
																c("Optimize" = "optimize_hidden_layer"),
																selected= c("optimize_hidden_layer"))
		      ),
		      column(3,
		        h3('Autoregression', style = 'margin-bottom: 20px;'),
		        radioButtons('aRModelName', 'Arima Models',
		          c(AR='AR', AutoArima='AutoArima', ManualAutoArima='ManualAutoArima', ManualAR='ManualAR'), 'AR')
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
					tabPanel("MLP with Hidden Layer",
							dataTableOutput("rsnns_mlp_tab"),
							plotOutput('mlp_plot')
					),
					tabPanel("MLP without Hidden Layer",
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
			          #tabPanel('NN Dif. wrt HL',
			          #  dataTableOutput('neuralNetworkDifferenceWRTHiddenLayers')
			          #),
			          tabPanel('Model Prodictions',
			            fluidRow(
			              column(width = 5,
			                selectInput('model1Select', 'Model 1',
			                  c('Auto-Regeressives Modell' = 'ar',
			                    'Neural Netzwerk: Eigenes Modell für jedes' = 'nnfe',
			                    'Neural Netzwerk: Eigenes Modell für jedes mit Hidden Layer' = 'nnfeh',
			                    'Neural Netzwerk: Ein Modell für alle' = 'nnfa',
			                    'Neural Netzwerk: Ein Modell für alle mit Hidden Layer' = 'nnfah',
			                    'Jordan-Netzwerk' = 'jordan',
			                    'Elman-Netzwerk' = 'elman',
			                    'MLP' = 'mlp',
			                    'MLP mit Hidden Layer' = 'mlph'), selected ='ar')
			              ),
			              column(width = 5,
			                selectInput('model2Select', 'Model 2',
			                  c('Auto-Regeressives Modell' = 'ar',
			                    'Neural Netzwerk: Eigenes Modell für jedes' = 'nnfe',
			                    'Neural Netzwerk: Eigenes Modell für jedes mit Hidden Layer' = 'nnfeh',
			                    'Neural Netzwerk: Ein Modell für alle' = 'nnfa',
			                    'Neural Netzwerk: Ein Modell für alle mit Hidden Layer' = 'nnfah',
			                    'Jordan-Netzwerk' = 'jordan',
			                    'Elman-Netzwerk' = 'elman',
			                    'MLP' = 'mlp',
			                    'MLP mit Hidden Layer' = 'mlph'), selected = 'nnfe')
			              ),
			              column(width = 2,
			                actionButton('compareModels', 'Vergleichen')
			              )
			            ),
			            dataTableOutput('ModelPredictionsCompareTable')
			          ),
			         tabPanel('CPU time',
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
