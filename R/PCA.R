#' Principal Component Analysis
#'
#' This function perform PCA with one step code or step-by-step.
#'
#' @param data Data matrix
#' @param label Whether use the label in PCA plot or not Defaults to FALSE
#' @param main The title in plot Defaults to "PVCA plot"
#' @import ggfortify
#' @author WKL
#' @keywords PCA
#' @return PCA plot
#' @examples
#' normalPCA(data, main="PCA plot")
#' @export normalPCA

#step-by-step pca plot
normalPCA <- function(data, label=FALSE, main="PCA plot"){
	pcdata <- princomp(data,cor=FALSE)
	# pdf("pca1.pdf")
	plot(pcdata$loadings,pch=18,main=main)
	if (label){
		text(pcdata$loadings,rownames(pcdata$loadings),pos=4,cex=.7)
	}
	# dev.off()
}

#' Principal Component Analysis
#'
#' This function perform PCA with one step code or step-by-step.
#'
#' @param data Data matrix
#' @param trait Sample information for PCA plot
#' @param colour One of the trait for colour in PCA plot
#' @param label Whether use the label in PCA plot or not Defaults to FALSE
#' @author WKL
#' @keywords PCA
#' @return PCA plot
#' @export
#' @examples
#' onestepPCA(data, trait, colour="Gender", label=FALSE)
#' @export

#one step pca auto-plot
onestepPCA <- function(data, trait, colour, label=FALSE){
	datat <- t(data) 
	alldata <- data.frame(datat,trait)
	# suppressMessages(library(ggfortify))
	pcaplot <- ggplot2::autoplot(prcomp(alldata[,c(1:nrow(data))]),data=alldata,colour=colour,label=label)
	# pdf("pca2.pdf")
	plot(pcaplot)
	# dev.off()
}
