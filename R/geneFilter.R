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
