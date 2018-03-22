#' Shiny hierarchical clustering 
#'
#' A simple \pkg{shiny} app to show outliers based on hierarchical clustering on local web browser.
#'
#' @param data Data matrix
#' @param traitNum Sample information which must be numeric for cluster plot Defaults to NULL
#' @param scale Whether scale is used for the data Defaults to FALSE
#' @param cutheight The init threshold for outliers Defaults to 50000
#' @param minSize The minimum cluster size for outliers Defaults to 10
#' @import shiny
#' @author WKL
#' @keywords cluster, shiny
#' @return a local webpage for visualization of hcluster
#' @examples
#' shinyHcluster(data, trait_num, cutheight=1000)
#' @export

shinyHcluster <- function (data, traitNum=NULL, scale=FALSE, cutheight=50000, minSize=10) {
	server <- function(input, output, session) {
		#updateNumericInput(session, "mycutoff", value = isolate(input$mycutoff))
		#ifelse(is.null(cutHeight()), cutheight <- cutheight, cutheight <- cutHeight())
		#observeEvent(input$updateButton,{ifelse(is.null(input$mycutoff), cutheight <- cutheight, cutheight <- input$mycutoff)
		#	})
		v <- reactiveValues(cutheight = cutheight)
  		observeEvent(input$updateButton, {v$cutheight <- input$mycutoff})
		output$plot <- renderPlot({
			#if (is.null(v$data)) return()
	    	outlier_hcluster(data, traitNum, scale, v$cutheight, minSize)
	  		})
		observe({
			out <- outlier_hcluster(data, traitNum, scale, v$cutheight, minSize, FALSE, FALSE)
			txt <- paste("There are ",sum(out)," outliers samples based on hierarchical clustering above cutoff", sep="")
    		updateTextInput(session, "outliers", value=txt)
  		})
	  	
	}

	ui <- basicPage(
	  
	    h3("The hcluster plot"),
	    numericInput("mycutoff", "CutHeight: ", cutheight),
	    actionButton("updateButton", "Update cutHeight"),
	    textInput("outliers", "Outliers: "),
	    plotOutput('plot')
	)

	shinyApp(ui = ui, server = server)
}

#' Shiny hierarchical clustering 2
#'
#' A simple \pkg{shiny} app to show outliers based on hierarchical clustering on local web browser.
#'
#' @param data Data matrix
#' @param traitNum Sample information which must be numeric for cluster plot Defaults to NULL
#' @param scale Whether scale is used for the data Defaults to FALSE
#' @param cutheight The init threshold for outliers Defaults to 50000
#' @param minSize The minimum cluster size for outliers Defaults to 10
#' @import shiny
#' @author WKL
#' @keywords cluster, shiny
#' @return a local webpage for visualization of hcluster
#' @examples
#' shinyHcluster2(data, trait_num, cutheight=1000)
#' @export

shinyHcluster2 <- function (data, traitNum=NULL, scale=FALSE, cutheight=50000, minSize=10) {
	server <- function(input, output, session) {
		#updateNumericInput(session, "mycutoff", value = isolate(input$mycutoff))
		#ifelse(is.null(cutHeight()), cutheight <- cutheight, cutheight <- cutHeight())
		#observeEvent(input$updateButton,{ifelse(is.null(input$mycutoff), cutheight <- cutheight, cutheight <- input$mycutoff)
		#	})
		v <- reactiveValues(cutheight = cutheight)
  		observeEvent(input$updateButton, {v$cutheight <- input$mycutoff})
		output$plot <- renderPlot({
			#if (is.null(v$data)) return()
	    	outlier_hcluster(data, traitNum, scale, v$cutheight, minSize)
	  		})
		output$outliers <- renderText({
			init <- outlier_hcluster(data, traitNum, scale, v$cutheight, minSize, FALSE, FALSE)
			return(paste("There are ",sum(init)," outliers samples based on hierarchical clustering above cutoff", sep=""))
  		})
		#observe({
		#	out <- outlier_hcluster(data, traitNum, scale, v$cutheight, minSize, FALSE, FALSE)
		#	txt <- paste("There are ",sum(out)," outliers samples based on hierarchical clustering above cutoff", sep="")
    	#	updateTextInput(session, "outliers", value=txt)
  		#})
	  	output$mytable <- renderTable({
	  		init <- outlier_hcluster(data, traitNum, scale, v$cutheight, minSize, FALSE, FALSE)
	  		inittable <- cbind(colnames(data)[init],colMeans(data[,init]))
	  		colnames(inittable) <- c("SampleID","colMeans")
    		return(inittable)
    	})
	}

	ui <- fluidPage(theme=shinythemes::shinytheme("cosmo"),
 		titlePanel("Shiny hierarchical clustering"),
  		sidebarLayout(
    		sidebarPanel(
			h3("Input the cutheight for outliers removing"),
      		numericInput("mycutoff", "CutHeight: ", cutheight),
      		actionButton("updateButton", "Update cutHeight")
      		), 
	    mainPanel(
      		tabsetPanel(
        	tabPanel("Plot", plotOutput("plot")), 
        	tabPanel("Outliers", verbatimTextOutput("outliers")), 
        	tabPanel("Table", tableOutput("mytable"))
      		)
    	)
  		)
	)

	shinyApp(ui = ui, server = server)
}
