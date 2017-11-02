library(shiny)
library(shinydashboard)
library(plotly)

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB
options(max.print=10)

ui <- dashboardPage(
	dashboardHeader(title = "FPADB"),
	
	dashboardSidebar(
		sidebarMenu(id="tabs",
			menuItem("Daten", tabName = "data", icon=icon("upload"), selected=TRUE),
			menuItem("SAX", icon=icon("sort-alpha-asc"),
				menuSubItem("Anwendung", tabName = "sax", icon = icon("list-ul")),
				menuSubItem("Parameterbestimmung", tabName = "statistic", icon = icon("pie-chart"))
			)
		)
	),
	
	dashboardBody(
		tabItems(
			tabItem(tabName = "data",
				fluidRow(
					column(width = 2,
						box(width = NULL,
							#Header Zeile
							checkboxInput('headerCheckbox', 'Header', TRUE),
							#Trennzeichen
							radioButtons('separatorRadioButton', 'Trennzeichen',
								c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),
							br(),
							fileInput('dataFile', h5(strong('Bitte wählen Sie eine CSV Datei aus.')),
								accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
							)
						),
						uiOutput("meteridSelectBox")
						),
					
					column(width = 10,
						box(width = NULL,
							collapsible = TRUE,
							title = "originale Zeitreihe", status = "primary", solidHeader = TRUE,
							tabBox( width = NULL,
								tabPanel(h5("Plot"),
									plotlyOutput("distPlot", height="500px")
								),
								tabPanel(h5("Tabelle"),
									dataTableOutput("table")
								)
							))
					)
				)
			),
			tabItem(tabName = "sax",
				fluidRow(
					column(width = 3,
						box( width = NULL,
							tabPanel(h5("Parameter"),
								uiOutput("motif_nsyms_input"),
								uiOutput("motif_asize_input"),
								actionButton("gosax", "Go!"))
						)
					),
					column(width = 9,
						box(width = NULL,
							splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot_sax",height="50%"), plotlyOutput("plot_treppe",height="50%")),
							collapsible = TRUE,
							title = "SAX-Plot", status = "primary", solidHeader = TRUE),
						box(width = NULL,
							plotlyOutput("plot_box"),
							title = "String", status = "primary", solidHeader = TRUE),
						# box(width = NULL, plotOutput("plot_sym",height="500px"), collapsible = TRUE,
						#       title = "Symbolic", status = "primary", solidHeader = TRUE),
						box(width = NULL,
							splitLayout(cellWidths = c("30%", "70%"), tableOutput("statisic_1"), tableOutput("statisic_2")), collapsible = TRUE,
							title = "Statistik", status = "primary", solidHeader = TRUE)#,
					# box(width = NULL,
					#      verbatimTextOutput("rmse"),
					#      plotlyOutput("plot_rmse",height="500px"), collapsible = TRUE,
					#dataTableOutput("org_rmse_table"),
					#      title = "Plot-RMSE", status = "primary", solidHeader = TRUE)
					)
				)),
			
			tabItem(tabName = "statistic",
				fluidRow(
					column(width = 4,
						tabBox( width = NULL,
							tabPanel(h5("Parameter"),
								uiOutput("o_wert"),
								verbatimTextOutput("laenge_txt"),
								actionButton("goButton", "Go!")
							),
							tabPanel(h5("Querschnitt"),
								uiOutput("nsyms_input"),
								uiOutput("asize_input"),
								actionButton("goQuer", "Querschnitt!")
							#  uiOutput("max_string_input"),
							#  uiOutput("max_alpha_input"),
							#  br(),
							)
						
						)
					),
					column(width = 8,
						# box(  width = NULL,
						#       collapsible = TRUE,
						#       title = "Treppenfunktion", status = "primary", solidHeader = TRUE,
						#       verbatimTextOutput("rmse_step_txt"),
						#       verbatimTextOutput("smape_step_txt"),
						#       plotlyOutput("plot_step_ar")
						#),
						box(  width = NULL,
							collapsible = TRUE,
							title = "Optimale Parameterpaare", status = "primary", solidHeader = TRUE,
							tabBox( width = NULL,
								tabPanel(h5("RMSE"),
									verbatimTextOutput("optim_kost_1"),
									dataTableOutput("table_o_1")
								),
								tabPanel(h5("sMAPE"),
									verbatimTextOutput("optim_kost_2"),
									dataTableOutput("table_o_2")
								),
								tabPanel(h5("Standardabweichung"),
									verbatimTextOutput("optim_kost_3"),
									dataTableOutput("table_o_3")
								)
							)
						#plotlyOutput("plot_optim_1"),
						#plotlyOutput("plot_optim_2"),
						),
						
						box(  width = NULL,
							collapsible = TRUE,
							title = "RMSE", status = "primary", solidHeader = TRUE,
							
							tabBox( width = NULL,
								tabPanel(h5("Plot"),
									plotlyOutput("plot_grid_rmse")
								),
								tabPanel(h5("Wertetabelle"),
									dataTableOutput("table_grid_rmse")
								),
								tabPanel(h5("3D-Darstellung"),
									plotlyOutput("plot_3d_step_1"),
									plotlyOutput("plot_a_rsme_step"),
									plotlyOutput("plot_s_rsme_step")
								)
							)
						),
						box(  width = NULL,
							collapsible = TRUE,
							title = "sMAPE", status = "primary", solidHeader = TRUE,
							tabBox( width = NULL,
								tabPanel(h5("Plot"),
									plotlyOutput("plot_grid_smape")
								),
								tabPanel(h5("Wertetabelle"),
									dataTableOutput("table_grid_smape")
								),
								tabPanel(h5("3D-Darstellung"),
									plotlyOutput("plot_3d_step_smape"),
									plotlyOutput("plot_a_smape_step"),
									plotlyOutput("plot_s_smape_step")
								
								)
							)
						),
						box(  width = NULL,
							collapsible = TRUE,
							title = "Standardabweichung", status = "primary", solidHeader = TRUE,
							tabBox( width = NULL,
								tabPanel(h5("Plot"),
									plotlyOutput("plot_grid_abw")
								),
								tabPanel(h5("Wertetabelle"),
									dataTableOutput("table_grid_abw")
								),
								tabPanel(h5("3D-Darstellung"),
									plotlyOutput("plot_3d_as",height="500px"),
									plotlyOutput("plot_alphabet",height="500px"),
									plotlyOutput("plot_string",height="500px")
								)
							)),
						
						#   box(  width = NULL,
						#          collapsible = TRUE,
						#          title = "RSME vs. sMAPE", status = "primary", solidHeader = TRUE,
						#          tabBox( width = NULL,
						#                  tabPanel(h5("1"),
						#                  plotlyOutput("kost_rmse"),
						#                  plotlyOutput("kost_smape")
						#                  ),
						#                  tabPanel(h5("2"),
						#                  dataTableOutput("table_rmse_smape")
						#                  )
						#          )
						#          ),
						box(  width = NULL,
							collapsible = TRUE,
							title = "RMSE und sMAPE", status = "primary", solidHeader = TRUE,
							tabBox( width = NULL,
								tabPanel(h5("ueber komp. Kosten"),
									plotlyOutput("kost_rmse"),
									plotlyOutput("kost_smape")
								),
								tabPanel(h5("Wertetabelle"),
									dataTableOutput("table_rmse_smape")
								)
							))
					
					))),
			
			tabItem(tabName = "motifs",
				fluidRow(
					column(width = 4,
						box(numericInput("wsize",
								"Fenster:",
								value = 3),
							numericInput("msize",
								"Maske Fenster rendom projection:",
								value = 3) )
					),
					column(width = 4,
						box(width = NULL, plotOutput("test_plot",height="500px"), collapsible = TRUE,
							title = "Plot-RMSE", status = "primary", solidHeader = TRUE)
					))
			)
		)
	)
)

server <- function(input, output) {
	dataset <- reactive({
			file <- input$dataFile
			
			if (is.null(file))
			{
				return(NULL)
			}
			
			df <- read.csv(file$datapath, header=input$headerCheckbox, sep=input$separatorRadioButton)
			split(df, f = df$meterid)
		})
	
	output$meteridSelectBox <- renderUI({
			df <- dataset()
			
			if (is.null(df))
			{
				return(NULL)
			}
			
			box(width = NULL,
				selectInput("meteridSelect", "Datensatz", names(df))
			)
		})
	
	output$distPlot <- renderPlotly({
			df <- dataset()
			meterid <- input$meteridSelect
			
			if (is.null(df) || is.null(meterid))
			{
				return(NULL)
			}
			
			data <- data.frame(day = df[[meterid]]$day, consumption = df[[meterid]]$consumption)
			
			p <- plot_ly(data, x = ~day, y = ~consumption, type = 'scatter', mode = 'lines')
			p$elementId <- NULL	# workaround for the "Warning in origRenderFunc() : Ignoring explicitly provided widget ID ""; Shiny doesn't use them"
			p
		})
}
app <- shinyApp(ui, server)
print(app)
