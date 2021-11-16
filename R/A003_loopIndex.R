#' loopIndex
#'
#' Calculates i modulo n, but returns numbers in the range from 1 to n instead of 0 to n-1.
#' @param i a vector
#' @param n a number
#' @return Returns the vector i modulo n in the range 1 to n
#' @export
#' @examples
#' loopIndex(1:10,4)
loopIndex=function(i,n){
	answer=i%%n
	for(j in 1:length(answer)){
		if(answer[j]==0){answer[j]=n}
	}
	return(answer)
}