#' spiral
#'
#' Creates a spiral.
#' @param numLines the number of lines spiralling out from the center
#' @param x,y co-ordinates for the centre of the spiral
#' @param maxRadius the size of the spiral
#' @param numPoints the number of points. [CHECK per line or per spiral??]
#' @param numTurns the number of times a line goes around the centre of the spiral.
#' @param clockwise logical. Sets the direction or rotation. Defaults to FALSE / anti-clockwise.
#' @param ... parameters passed to makePolar
#' @return Returns a component
#' @export
#' @examples
#' pruneStart(1:10,4)
makeSpiral=function(numLines=1,x=0.5,y=0.5,maxRadius=0.49,numPoints=100,numTurns=3.5,clockwise=FALSE,...){
	lineAngles=seq(from=0,to=2*pi,length.out=numLines+1)[-(numLines+1)]
	listLines=vector("list",numLines)
	signRotation=1
	if(clockwise){signRotation=-1}
	fixedDistFun=paste("function(x){return(",maxRadius,"*x)}")
	distanceFunction=eval(parse(text=fixedDistFun))
	fixedBearingFun=paste("function(x){return(",signRotation,"*x*2*pi*",numTurns,")}")
	bearingFunction=eval(parse(text=fixedBearingFun))	
	parameterSpace=seq(0,1,length.out=numPoints)
	spiral=makePolar(x=x,y=y,
					distanceFunction=distanceFunction,
					bearingFunction=bearingFunction,
					parameterSpace=parameterSpace,
					rotation=lineAngles,
					numLines=numLines,...)
	return(spiral)
}
