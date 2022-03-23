#' Cyclic Cellular Automata
#'
#' @description
#' Functions to calculate cyclic cellular automata
#' @param input a matrix
#' @param image a matrix
#' @param numSteps
#' @param maxNum The number of values a cell can have
#' @param nbhdType The type of neighbourhood
#' @param range the size of the neighbourhood
#' @param threshold the requirement to advance a cell
#' @param xSize,ySize size of a randomly generated starting grid
#' @param .plotting logical vector of length two. Used for timing the functions runCCA and matrixToColours. 
#' @param ... parameters passed to the component and the component functions. See "details" for more information.
#' @return makeCCA returns a component. runCCA returns a list containing the image. randomInitialCCA returns a matrix.
#' @name cyclicCellularAutomata

#' @rdname cyclicCellularAutomata
#' @export
runCCA=function(input,image=NULL,numSteps=10,maxNum=7,nbhdType="VN",range=1,threshold=1){
	answer=input
	if(!is.null(image)){answer=image}
	numR=nrow(input)
	numC=ncol(input)
	if(numSteps>0){
		for(i in 1:numSteps){
			temp=neighbourhood(answer,type=nbhdType,range=range)
			for(j in 1:numR){
				for(k in 1:numC){
					if(sum((answer[j,k]%%maxNum+1)==temp[j,k,])>=threshold){
						if(answer[j,k]<maxNum){answer[j,k]=answer[j,k]+1}else{answer[j,k]=1}
					}
				}
			}
		}
	}
	return(list(image=answer))
}
#' @rdname cyclicCellularAutomata
#' @export
randomInitialCCA=function(maxNum=7,xSize=100,ySize=100){
	return(list(input=randomMatrix(maximum=maxNum,numRow=xSize,numCol=ySize)))
}
#' @rdname cyclicCellularAutomata
#' @export
makeCCA=function(input=NULL,.abcd=c("a","d"),...){
	if(is.null(input)){
		answer=makeImage(...)+functionList(fun=list("randomInitialCCA","runCCA","matrixToColours"),.abcd=c("a",.abcd),...)
	}else{
		answer=makeImage(input=input,...)+functionList(fun=list("runCCA","matrixToColours"),.abcd=.abcd,...)
	}
	return(answer)
}
