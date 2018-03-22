#' Sample filter based on connectivity z-socres
#'
#' This function remove outliers based on network connectivity z-scores.
#'
#' @param data Data matrix
#' @param onetrait One sample information for plot Defaults to NULL
#' @param sdout The threshold for outliers Defaults to 2
#' @param label Whether use the label in plot or not Defaults to FALSE
#' @param plot Whether output the plot or not Defaults to TRUE
#' @author WKL
#' @keywords connectivity, outliers
#' @return outliers
#' @examples
#' outlier_connectivity(data, trait$Gender, sdout=2, label=FALSE)
#' @export

outlier_connectivity <- function(data, onetrait=NULL, sdout=2, label=FALSE, plot=TRUE){
	normadj <- (0.5+0.5*WGCNA::bicor(data, use='pairwise.complete.obs'))^2
	netsummary <- WGCNA::fundamentalNetworkConcepts(normadj);
	K <- netsummary$Connectivity; Z.K <- (K-mean(K))/sqrt(var(K))
	# C <- netsummary$ClusterCoef; Z.C = (C - mean(C))/sqrt(var(C))
	outliers <- (Z.K > mean(Z.K)+sdout*sd(Z.K))|(Z.K < mean(Z.K)-sdout*sd(Z.K))
	print(paste("There are ",sum(outliers)," outliers samples based on a bicor distance sample network connectivity standard deviatiion above sdout", sep="")); 
	# print(colnames(data)[outliers]); 
	# print(table(outliers))
	if (plot){
		if (is.null(onetrait)){
			plot(Z.K, pch=19, main="Outlier detection", ylab="Network connectivity (z score)")
			abline(h=-2, lty=2)
			if (label){
				text(1:length(Z.K),Z.K,colnames(data),cex=1,pos=1)
			}
		} else {
			plot(Z.K, col = as.numeric(onetrait), pch=19, main="Outlier detection", ylab="Network connectivity (z score)")
			# plot(Z.K, col = as.numeric(outliers)+1, pch=19, main="Outlier detection", ylab="Network connectivity (z score)")
			if (label){
				text(1:length(Z.K),Z.K,colnames(data),cex=1,pos=1,col=as.numeric(onetrait))
			}
			abline(h=-2, lty=2)
			# abline(h=c(2,-2), lty=2)
			legend("bottomleft",legend = levels(onetrait), col=1:7,pch=19,cex=.7)
			# legend("bottomleft",legend = c("non-outliers","outliers"), col=1:2,pch=19,cex=.7)
		}
	}
	outliers
}

#' Sample filter based on hierarchical clustering
#'
#' A simple \pkg{shiny} app to show outliers based on hierarchical clustering on local web browser.
#'
#' @param data Data matrix
#' @param traitNum Sample information which must be numeric for cluster plot Defaults to NULL
#' @param scale Whether scale is used for the data Defaults to FALSE
#' @param cutheight The threshold for outliers Defaults to 50000
#' @param minSize The minimum cluster size for outliers Defaults to 10
#' @param plot Whether output the plot or not Defaults to TRUE
#' @param print Whether print the result or not Defaults to TRUE
#' @author WKL
#' @keywords cluster, shiny
#' @return a local webpage for visualization of hcluster
#' @examples
#' outlier_hcluster(data, trait_num, cutheight=50000)
#' @export

outlier_hcluster <- function(data, traitNum=NULL, scale=FALSE, cutheight=50000, minSize=10, plot=TRUE, print=TRUE){
	#WGCNA hcluster (include scale)
	datat = t(data)
	if (scale){
		hc = hclust(dist(scale(datat)), method = "average");
	} else{
		hc = hclust(dist(datat), method = "average");
	}
	#hcd = as.dendrogram(hc)
	if (plot){
		if (is.null(traitNum)){
			#sizeGrWindow(12,9)
			#pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9);
			par(cex = 0.6);
			par(mar = c(0,4,2,0))
			plot(hc, main = "Sample cluster", sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2, hang = -1) #hang could remove
			#dev.off()
			# Plot a line to show the cut
			abline(h = cutheight, col = "red");
		} else{
			#hcluster and trait heatmap
			#Convert traits to a color representation: white means low, red means high, grey means missing entry
			traitColors = WGCNA::numbers2colors(traitNum, signed = FALSE);
			# Plot the sample dendrogram and the colors underneath.
			WGCNA::plotDendroAndColors(hc, traitColors, abHeight = cutheight, groupLabels = names(traitNum), main = "Samples dendrogram and trait heatmap")
		}
	}
	# Determine cluster under the line
	clust = WGCNA::cutreeStatic(hc, cutHeight = cutheight, minSize = minSize)
	names(clust) <- colnames(data)
	table(clust)
	# clust 1 contains the samples we want to keep.
	outliers = (clust!=1)
	if (print){
		print(paste("There are ",sum(outliers)," outliers samples based on hierarchical clustering above cutoff", sep="")); 
	}

	outliers
}

#' Sample filter based on PCA
#'
#' This function remove outliers based on principal component analysis (PCA).
#'
#' @param data Data matrix
#' @param trait Sample information for PCA plot Defaults to NULL
#' @param colour One of the trait for colour in PCA plot Defaults to NULL
#' @param label Whether use the label in PCA plot or not Defaults to FALSE
#' @param pcout The threshold for outliers Defaults to 0.3
#' @param plot Whether output the plot or not Defaults to TRUE
#' @author WKL
#' @keywords cluster
#' @return outliers
#' @examples
#' outlier_PCA(data, trait, colour="Gender", pcout=0.3)
#' @export

outlier_PCA <- function(data, trait=NULL, colour=NULL, label=FALSE, pcout=0.3, plot=TRUE){
	pca = prcomp(data)
	outliers = (abs(pca$rotation[,1])>pcout) | (abs(pca$rotation[,2])>pcout)
	print(paste("There are ",sum(outliers)," outliers samples based on PC1 and PC2 above pcout", sep="")); 
	if (plot){
		if (is.null(trait)){
			normalPCA(data,label)
		} else{
			onestepPCA(data, trait, colour, label)
		}
	}
	outliers
}
