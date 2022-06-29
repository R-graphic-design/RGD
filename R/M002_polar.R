#' Polar Co-ordinates
#'
#' @description
#' Functions to convert polar co-ordinates into cartesean co-ordinates and to make a component that uses functions that output polar co-ordinates.
#' @param type the type/graphics function of the component.
#' @param x,y the center polar co-ordinates are drawn from.
#' @param distanceFunction a function that returns distances from x,y
#' @param bearingFunction a function that returns bearings from x,y
#' @param parameterSpace a vector of parameters passed to the distance and bearing functions.
#' @param rotation an additional rotation. Defaults to 0.
#' @param numLines divides the answer into a number of lines seperated by NAs for use with lines().
#' @param ... parameters passed to the component and the component functions. See "details" for more information.
#' @details
#' @return runPolar returns a list with x and y components. makePolar returns a component.
#' @family polarConversion
#' @seealso
#' @examples
#' print(1+1)
#' @name makePolar
NULL
#> NULL

#' @rdname makePolar
#' @export
runPolar=function(x=0.5,y=0.5,distanceFunction=function(x){x/2},bearingFunction=function(x){x*2*pi},parameterSpace=seq(0,1,length.out=100),rotation=0,numLines=1){
	numPoints=length(parameterSpace)
	if(numLines>1){
		if(class(parameterSpace)=="list"){
				parameterSpace=c(parameterSpace,recursive=TRUE)
				numPoints=length(parameterSpace)
		}else{
				numPoints=length(parameterSpace)*numLines
		}
	}
	if(length(parameterSpace)<numPoints){
		if(numPoints%%length(parameterSpace)==0){
			parameterSpace=rep(parameterSpace,times=numPoints/length(parameterSpace))
		}else{
			warning("length(parameterSpace) doesn't divide numPoints")
		}
	}
	xCoords=rep(0,numPoints)
	yCoords=rep(0,numPoints)
	if(numPoints%%length(x)==0){
		xCoords=rep(x,each=numPoints/length(x))
	}else{
		warning("using x=0, length(x) doesn't divide length(parameters)")
	}
	if(numPoints%%length(y)==0){
		yCoords=rep(y,each=numPoints/length(y))
	}else{
		warning("using y=0, length(y) doesn't divide length(parameters)")
	}
	distanceFunctions=distanceFunction
	if(!is.list(distanceFunction)){distanceFunctions=list(distanceFunction)}
	bearingFunctions=bearingFunction
	if(!is.list(bearingFunction)){bearingFunctions=list(bearingFunction)}
	print(length(parameterSpace))
	print(numPoints)
	print(numLines)
	#print(xCoords)
	#print(yCoords)
	for(i in 1:numPoints){
		lineNumber=ceiling(i*numLines/numPoints)
		#print(lineNumber)
		#print(rotation[lineNumber])
		tempx=xCoords[i]
		tempy=yCoords[i]
		for(j in 1:length(distanceFunctions)){
			tempPoint=polar2cart(tempx,tempy,do.call(distanceFunctions[[j]],list(parameterSpace[i])),do.call(bearingFunctions[[j]],list(parameterSpace[i]))+rotation[lineNumber])
			tempx=tempPoint$x
			tempy=tempPoint$y
		}
		xCoords[i]=tempx
		yCoords[i]=tempy
	}
	answer=list(x=xCoords,y=yCoords)
	if(numLines>1){
		answerWithNAs=list(x=c(),y=c())
		for(i in 1:numLines){
			if(i>1){
				answerWithNAs$x=c(answerWithNAs$x,NA)
				answerWithNAs$y=c(answerWithNAs$y,NA)
			}
			answerWithNAs$x=c(answerWithNAs$x,answer$x[(1:(numPoints/numLines))+(numPoints/numLines)*(i-1)])
			answerWithNAs$y=c(answerWithNAs$y,answer$y[(1:(numPoints/numLines))+(numPoints/numLines)*(i-1)])
		}
		return(answerWithNAs)
	}else{
		return(answer)
	}
}


#' @rdname makePolar
#' @export
makePolar=function(type="lines",...){
	if("parameterSpace"%in%names(list(...))){
		return(new("component",type=type,...)+action.data("runPolar",...))
	}else{
		return(new("component",type=type,...)+action.data(c("makeTSpace","runPolar"),...))
	}
}