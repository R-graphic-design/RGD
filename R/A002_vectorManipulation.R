#' pruneStart
#'
#' Takes a vector and returns the end of that vector.
#' @param x an input vector
#' @param lengthToKeep determines the length of the output
#' @return Returns a vector of a given length from the end of the input vector.
#' @export
#' @examples
#' pruneStart(1:10,4)

#Used by attractors like DeJong, Clifford, etc.
pruneStart=function(x,lengthToKeep){
	return(x[(length(x)-lengthToKeep+1):length(x)])
}