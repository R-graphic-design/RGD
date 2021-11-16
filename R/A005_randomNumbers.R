#' random number functions
#'
#' @description
#' Functions to return a either a vector or a matrix filled with random integers from 1 to maximum.
#' @param lengthOut the length of the vector required.
#' @param numCol, numRow the dimensions of the matrix required. Both default to 1.
#' @param maximum the largest possible random integer
#' @return random returns a vector of random numbers, whilst randomMatrix returns a matrix of random numbers.
#' @examples
#' random(10,5)
#' randomMatrix(3,6,10)
#' @name randomNumbers

#' @rdname randomNumbers
#' @export
random<-function(lengthOut,maximum){
	return(floor(runif(lengthOut)*maximum)+1)
}

#' @rdname randomNumbers
#' @export
randomMatrix=function(numCol=1,numRow=1,maximum){
return(matrix(random(numCol*numRow,maximum),numRow,numCol))
}