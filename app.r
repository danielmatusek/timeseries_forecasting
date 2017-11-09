library(shiny)

options(shiny.maxRequestSize = 50*1024^2)	# Upload up to 50 MiB

shinyApp(ui, server)
