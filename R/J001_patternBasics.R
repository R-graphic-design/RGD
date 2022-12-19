#' Pattern Building Blocks
#'
#' @description
#' Functions to draw a small square in a pattern step by step.
#' @param x0,x1,y0,y1 co-ordinates for the top, bottom, left and right of the square.
#' @param border colour for border lines. see details.
#' @param background the background colour for the square.
#' @param colour the colour of a line added to the pattern.
#' @param x,y co-ordinates for a line added to the pattern.
#' @param pattern a pattern to add a line or polygon to.
#' @param ... parameters passed to component().
#' @details
#' blankPattern creates a component with a single square, and the other functions add details.
#' @return
#' @family patterns
#' @seealso
#' @examples
#' print(1+1)
#' @name patternBasics
NULL

#' @rdname patternBasics
#' @export
blankPattern=function(x0,x1,y0,y1,border="black",background="white",...){
	answer=component(type="polygon",polygon_x=c(x0,x1,x1,x0),polygon_y=c(y0,y0,y1,y1),polygon_col=background,border=NA,...)
	if(length(border)==1){
		print("BORDER LENGTH 1")
		answer@data$border=border
	}else{
		if(!is.na(border[1])){
			answer=addLineToPattern(answer,x0,x0,y0,y1,border[1])
		}
		if(!is.na(border[2])){
			answer=addLineToPattern(answer,x0,x1,y1,y1,border[2])
		}
		if(!is.na(border[3])){
			answer=addLineToPattern(answer,x1,x1,y0,y1,border[3])
		}
		if(!is.na(border[4])){
			answer=addLineToPattern(answer,x0,x1,y0,y0,border[4])
		}
	}
	return(answer)
}

#' @rdname patternBasics
#' @export
addLineToPattern=function(pattern,x0,x1,y0,y1,colour){
	if("segments"%in%pattern@type){
		pattern@data$x0=c(pattern@data$x0,x0)
		pattern@data$x1=c(pattern@data$x1,x1)
		pattern@data$y0=c(pattern@data$y0,y0)
		pattern@data$y1=c(pattern@data$y1,y1)
		pattern@data$segments_col=c(pattern@data$segments_col,colour)
	}else{
		pattern@data$x0=x0
		pattern@data$x1=x1
		pattern@data$y0=y0
		pattern@data$y1=y1
		pattern@data$segments_col=colour
		pattern@type=c(pattern@type,"segments")
	}
	return(pattern)
}


#' @rdname patternBasics
#' @export
addPolygonToPattern=function(pattern,x,y,border,colour){
	pattern@data$polygon_x=c(pattern@data$polygon_x,NA,x)
	pattern@data$polygon_y=c(pattern@data$polygon_y,NA,y)
	pattern@data$polygon_col=c(pattern@data$polygon_col,colour)
	pattern@data$border=c(pattern@data$border,border)
	return(pattern)
}

