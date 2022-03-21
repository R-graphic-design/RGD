#' Julia Plots
#'
#' @description
#' Functions to run the calculations for Julia Plots.
#' @param size vector length 2 giving the dimensions of the matrix
#' @param xScale,yScale 
#' @param coefficient The c parameter in z^2+c. Vector length 2 representing the real and imaginary parts of the number c.
#' @param maxIter The number of steps at which a point is considered to be bounded.
#' @param maxBound The distance at which a point is considered to have escaped to infinity.
#' @param ... parameters passed to the component and the component function runJuliaPlot. See "details" for more information.
#' @details
#' @return
#' @examples
#' print(1+1)
#' @name juliaPlots
NULL
#> NULL

#' @rdname juliaPlots
#' @export
runJuliaPlot=function(size=c(1000,1000),xScale=c(-2,2),yScale=c(-2,2),coefficient=c(0.276,0),maxIter=100,maxBound=4){
	xSize=size[1]
	ySize=size[2]
	answer=matrix(rep(0,size[1]*size[2]),size[1])
	xMin=xScale[1]
	xMax=xScale[2]
	yMin=yScale[1]
	yMax=yScale[2]
	xScale=seq(xMin,xMax,length.out=xSize)
	yScale=seq(yMin,yMax,length.out=ySize)
	xCoefficient=coefficient[1]
	yCoefficient=coefficient[2]
	maxIteration=maxIter
	for(i in 1:xSize){
		for(j in 1:ySize){
			xTemp=xScale[i]
			yTemp=yScale[j]
			iteration=0
			while((xTemp^2+yTemp^2)<maxBound & iteration < maxIteration){
				oldX=xTemp
				temp=xTemp^2-yTemp^2
				xTemp=temp+xCoefficient
				yTemp=2*oldX*yTemp+yCoefficient
				iteration=iteration+1
			}
			if(iteration==maxIteration){
				answer[i,j]=maxIteration+1
			}else{
				answer[i,j]=iteration
			}
		}
	}
	return(list(image=answer+1))
}

#' @rdname juliaPlots
#' @export
makeJuliaPlot=function(...){
	answer=makeImage(...)+action.data(c("runJuliaPlot","matrixToColours"),...)
	return(answer)
}

