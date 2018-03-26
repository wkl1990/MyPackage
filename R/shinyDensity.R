#' Shiny density plot 
#'
#' A simple \pkg{shiny} app to show density plot of given sample on local web browser.
#'
#' @param data Data matrix
#' @param xlab The title used for the xlab of boxplot Defaults to \pkg{log2(value + .001)}
#' @param main The title for plot Defaults to \pkg{Density plot}
#' @param ylim The y-axis limitaiton for density plot Defaults to \pkg{c(0,0.5)}
#' @import shiny
#' @author WKL
#' @keywords density, shiny
#' @return a local webpage for visualization of density
#' @examples
#' shinyDensity(data, ylim=c(0,0.5))
#' @export

shinyDensity <- function(data, main="Density plot", xlab="log2(value + .001)", ylim=c(0,0.5)){
	server <- function(input, output, session) {

		observeEvent(input$mybutton,{
			data_select <- reactive(data[, c(isolate(input$sample))])
			output$plot <- renderPlot({
				density_log(data)
				line_log(data_select())
			})
		})
		observeEvent(input$All,{
			updateSelectInput(session, "sample", selected=colnames(data))
		})		
		observeEvent(input$Clear,{
			updateSelectInput(session, "sample", selected=character(0))
		})
		observeEvent(input$mybutton2,{
			choice=unlist(strsplit(isolate(input$myinput),","))
			updateSelectInput(session, "sample", selected=choice)
		})
	}

	ui <- basicPage(
		h3("Density plot"),
		selectInput("sample", "Samples:",colnames(data),selected=colnames(data)[1],multiple=TRUE),
		actionButton("mybutton", "Click to update plot"),
		textInput("myinput","Input the sample name(use ',' if choose multiple samples)"),
		actionButton("mybutton2", "Click to update select"),
		actionButton("All", "Select all sample"),
		actionButton("Clear", "Clear all sample"),
		plotOutput('plot')
	)

	shinyApp(ui = ui, server = server)
}

# Density plot 
density_log <- function(data, main="Density plot", xlab="log2(value + .001)", ylim=c(0,0.5)){
	if (is.null(ncol(data))){
		data_log <- log2(data+0.001)
		plot(density(data_log, na.rm=T),ylim=ylim, main=main, xlab = xlab)
	} else {
		i = 1
		data_log <- log2(data+0.001)
		plot(density((data_log[,i]), na.rm=T),ylim=ylim, main=main, xlab = xlab)
		for(i in 2:dim(data_log)[2]) {lines(density((data_log[,i]), na.rm=T), ylim=ylim)}
	}
}
# Add the line plot 
line_log <- function(data, ylim=ylim) {
	data_log <- log2(data+0.001)
	if (is.null(ncol(data))){
		lines(density(data_log, na.rm=T), col = "red",ylim=c(0,0.21))
	} else {
		for(i in 1:dim(data_log)[2]) {lines(density((data_log[,i]), na.rm=T), col = "red",ylim=c(0,0.21))}
	}
}

