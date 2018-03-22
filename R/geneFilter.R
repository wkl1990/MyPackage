#' Gene Filter
#'
#' This function perform gene filter based on expression matrix.
#'
#' @param data Data matrix
#' @param cutoff The cutoff defined the expressed gene Defaults to 0.1
#' @param percent The percentage of total sample used to set cutoff Defaults to 0.8
#' @author WKL
#' @keywords filter
#' @return Filtered data matrix
#' @examples
#' gene_filter(data, cutoff=0.1, percent=0.8)
#' @export

gene_filter <- function(data, cutoff=0.1, percent=0.8){
	data_sample = apply(data>cutoff, 1, sum)
	data_filter = data[data_sample>ncol(data)*percent,]
	data_filter
}

#' MAD Filter
#'
#' This function perform gene filter based on Median Absolute Deviation (MAD).
#'
#' @param data Data matrix
#' @param percent The percentage of total gene need to remove Defaults to 0.5
#' @param rmLower The \pkg{lower} MAD gene need to remove or remove \pkg{higher} if set to FALSE  Defaults to TRUE
#' @author WKL
#' @keywords MAD filter
#' @return Filtered data matrix based on MAD
#' @examples
#' mad_filter(data, percent=0.5, rmLower=TRUE)
#' @export

mad_filter <- function(data, percent=0.5, rmLower=TRUE){
	data_mad <- apply(data,1,mad)
	cutoff <- quantile(data_mad,percent)
	if (rmLower){
		data_filtermad <- data[which(data_mad>cutoff),]
	} else {
		data_filtermad <- data[which(data_mad<cutoff),]
	}
	data_filtermad
}
