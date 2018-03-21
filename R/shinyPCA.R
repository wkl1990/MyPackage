#' Shiny PCA
#'
#' A simple \pkg{shiny} app to show outliers based on PCA on local web browser.
#'
#' @param data Data matrix
#' @param onetrait One sample information for plot Defaults to NULL
#' @param sdout The threshold for outliers Defaults to 2
#' @author WKL
#' @keywords PCA, outliers
#' @return a local webpage for visualization of PCA
#' @examples
#' shinyPCA(data, onetrait)
#' @export

#APP37
shinyPCA <- function(data, onetrait=NULL){
	server <- function(input, output, session) {
		
		# global variable, what type of plot interaction
		interaction_type <- "click"
		
		# observe for user interaction and change the global interaction_type
		# variable
		observeEvent(input$user_click, interaction_type <<- "click")
		observeEvent(input$user_brush, interaction_type <<- "brush")
		# the PCA plot
		pca <- as.data.frame(prcomp(data)$rotation)
		output$plot <- renderPlot({
			if (is.null(onetrait)){
				ggplot(pca, aes(PC1,PC2))+geom_point(size=2)
			} else {
				ggplot(pca, aes(PC1,PC2))+geom_point(aes(color=onetrait),size=2)
			}
		})
	
		# generate the data to put in the table
		dat <- reactive({
			user_brush <- input$user_brush
			user_click <- input$user_click
		
			if(interaction_type == "brush") res <- brushedPoints(pca, user_brush)
			if(interaction_type == "click") res <- nearPoints(pca, user_click, threshold = 10, maxpoints = 1)
	
			return(res)
		})
	
		output$table <- DT::renderDataTable(DT::datatable(dat()[,c("PC1", "PC2")]))
		# return a list of UI elements
		output$my_output_UI <- renderUI({

			list(
				h4(style = "color:blue;", "My selection outliers list"),
				selectInput(inputId = "myselect", label="", choices = selections)
			)
			})

		# initial selections
		selections <- c("")
		# use observe event to notice when the user clicks the button
		# update the selection list. Note the double assignment <<-
		observeEvent(input$mybutton,{
			selections <<- c(rownames(dat()), selections)
			updateSelectInput(session, "myselect", choices = selections, selected = selections[1])
		})
}


	ui <- fluidPage(
		h3("Click or brush the plot and it will filter the table"),
		plotOutput("plot", click = "user_click", brush = "user_brush"),
		DT::dataTableOutput("table"),
		uiOutput("my_output_UI"),
		actionButton("mybutton", "Click to add to selections")
		)

	shinyApp(ui = ui, server = server)
}
