#' makePatternBlock
#'
#' @description
#' Functions to create a block of the same pattern repeated over and over. 
#' @param input
#' @param ... parameters passed to the component and the function runPatternBlock. See details.
#' @details
#' @return makePatternBlock creates a component whilst runPatternBlock takes in a component anc returns a list of components.
#' @family patterns
#' @seealso
#' @examples
#' print(1+1)
#' @name makePatternBlock
NULL

#' @rdname makePatternBlock
#' @export
runPatternBlock=function(input){
	patternfunction=input@data$patternfunction
	x0=input@data$x0
	if(is.null(x0)){x0=0}
	x1=input@data$x1
	if(is.null(x1)){x1=1}
	y0=input@data$y0
	if(is.null(y0)){y0=0}
	y1=input@data$y1
	if(is.null(y1)){y1=1}
	across=input@data$across
	if(is.null(across)){across=1}
	vertical=input@data$vertical
	if(is.null(vertical)){vertical=1}
	alternateColours=input@data$alternateColours
	if(is.null(alternateColours)){alternateColours=FALSE}
	backgroundBorder=input@data$backgroundBorder
	if(is.null(backgroundBorder)){backgroundBorder="black"}
	patternBorder=input@data$patternBorder
	if(is.null(patternBorder)){patternBorder="black"}
	background=input@data$background
	if(is.null(background)){background="white"}
	foreground=input@data$foreground
	if(is.null(foreground)){foreground="black"}
	onlySides=input@data$onlySides
	if(is.null(onlySides)){onlySides=TRUE}
	answer=list()
	parameters=list(x0=x0,x1=x1,y0=y0,y1=y1,patternBorder=patternBorder,backgroundBorder=backgroundBorder,background=background,foreground=foreground)
	if(length(backgroundBorder)==1){parameters$backgroundBorder=rep(backgroundBorder,4)}

	xPoints=seq(from=parameters$x0,to=parameters$x1,len=across+1)
	yPoints=seq(from=parameters$y0,to=parameters$y1,len=vertical+1)
	for(i in 1:across){
		parameters$x0=xPoints[i]
		parameters$x1=xPoints[i+1]
		for(j in 1:vertical){
			parameters$y0=yPoints[j]
			parameters$y1=yPoints[j+1]
			if(alternateColours){
				parameters$foreground=c(foreground,background)[((i+j)%%2)+1]
				parameters$background=c(foreground,background)[((i+j+1)%%2)+1]
			}
			if(onlySides){
			if(i>1){parameters$backgroundBorder[1]=NA}else{parameters$backgroundBorder[1]=backgroundBorder}
			if(i<across){parameters$backgroundBorder[3]=NA}else{parameters$backgroundBorder[3]=backgroundBorder}
			if(j>1){parameters$backgroundBorder[4]=NA}else{parameters$backgroundBorder[4]=backgroundBorder}
			if(j<vertical){parameters$backgroundBorder[2]=NA}else{parameters$backgroundBorder[2]=backgroundBorder}			
			}

			answer=c(answer,do.call(patternfunction,parameters))
		}
	}
	return(answer)
}

#' @rdname makePatternBlock
#' @export
makePatternBlock=function(...){
	answer=new("component",...)+action.object("runPatternBlock",...)
	return(answer)
}