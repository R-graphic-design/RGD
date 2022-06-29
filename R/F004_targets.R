#' makeTarget
#'
#' Creates a layer containing a series of concentric circles.
#' @param x,y co-ordinates for the centre of the targetColours
#' @param maxRadius size of largest circle
#' @param numberOfColours number of circles
#' @param .useData controls addColours and colourComponents.
#' @param ... passed to layer@data and function objects
#' @return Returns a layer with one component per circle.
#' @export
#' @examples
#' easyPlot(makeTarget(x=0,y=0,1,3)



makeTarget=function(x,y,maxRadius,numberOfColours,.useData=c(TRUE,FALSE),addColoursName="componentColours",...){
	answer=new("layer")+artdata(x=x,y=y,maxRadius=maxRadius,addColoursName=addColoursName,...)+functionList(c("addColours","colourComponents"),.useData=.useData,...)
	for(i in 1:numberOfColours){
		answer@components[[i]]=makeCircle(x=x,y=y,r=maxRadius*(1+(1-i)/numberOfColours))
	}
	return(answer)
}