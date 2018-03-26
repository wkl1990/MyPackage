#' Box plot and bar plot
#'
#' Box plot of log transformed data and bar plot of the mapping reads or total counts for quanlity control.
#'
#' @param data Data matrix
#' @param oneTrait One sample information which must be factor for box plot Defaults to NULL
#' @param ylab The title used for the ylab of boxplot Defaults to \pkg{log2(value+0.001)}
#' @param main The title for plot Defaults to \pkg{QC plot}
#' @param mapping_reads The mapping reads which match the data for bar plot Defaults to NULL
#' @param height The threshlod for bar plot Defaults to 5
#' @author WKL
#' @keywords boxplot, barplot, QC
#' @return a boxplot of the data
#' @examples
#' QC_boxplot(data, trait$Race, mapping_reads=(runif(65,min=1000000,max=10000000)))
#' @export

QC_boxplot <- function(data, oneTrait=NULL, ylab="log2(value+0.001)", main="QC plot", mapping_reads=NULL, height=5){
	if (is.null(mapping_reads)){
		boxplot_log(data, oneTrait, ylab=ylab, main=main)
	} else {
		par(mfrow=c(2,1))
		boxplot_log(data, oneTrait, ylab=ylab, main=main)
		barplot_reads(mapping_reads,oneTrait,height)
	}
}

boxplot_log <- function(data, oneTrait=NULL, ylab="log2(value+0.001)", main="QC plot"){
	data_log <- log2(data+0.001)
	boxplot(data_log,ylab=ylab,col=as.numeric(oneTrait),range=0,main=main,las=2)
	if (!is.null(oneTrait)){
		legend("topright",levels(oneTrait),fill=as.numeric(unique(oneTrait)))
	}
}

barplot_reads <- function(mapping_reads, oneTrait=NULL, height=5){
	barplot(mapping_reads/10^6,col=as.numeric(oneTrait), ylab="Mapping Reads (x10e6)")
	if (!is.null(oneTrait)){
		legend("topright",levels(oneTrait),fill=as.numeric(unique(oneTrait)), cex=.75)
	}
	abline(h=height,col="blue")
}

#' Density plot 
#'
#' Density plot of log transformed data for quanlity control.
#'
#' @param data Data matrix
#' @param oneTrait One sample information which must be factor for density plot Defaults to NULL
#' @param xlab The title used for the xlab of boxplot Defaults to \pkg{log2(value + .001)}
#' @param main The title for plot Defaults to \pkg{Density plot}
#' @param ylim The y-axis limitaiton for density plot Defaults to \pkg{c(0,0.5)}
#' @author WKL
#' @keywords density, QC
#' @return a density plot of the data
#' @examples
#' QC_density(data, trait$Race)
#' @export

QC_density <- function(data, oneTrait=NULL, main="Density plot", xlab="log2(value + .001)", ylim=c(0,0.5)){
	data_log <- log2(data+0.001)
	if (is.null(ncol(data))){
		plot(density(data_log, na.rm=T),ylim=ylim, main=main, xlab = xlab)
	} else {
		i = 1
		if (is.null(oneTrait)){
			plot(density((data_log[,i]), na.rm=T),ylim=ylim, main=main, xlab = xlab)
			for(i in 2:dim(data_log)[2]) {lines(density((data_log[,i]), na.rm=T), ylim=ylim)}
		} else {
			plot(density((data_log[,i]), na.rm=T),ylim=ylim, col = as.numeric(oneTrait[i]), main=main, xlab = xlab)
			for(i in 2:dim(data_log)[2]) {lines(density((data_log[,i]), na.rm=T), col = as.numeric(oneTrait[i]),ylim=ylim)}
			legend("topright", levels(oneTrait), cex=0.7, col = 1:3, pch=19)
		}
	}
}
