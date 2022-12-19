#' Square Patterns
#'
#' @description
#' Functions to create square patterns
#' @param x0,x1,y0,y1 co-ordinates for the top, bottom, left and right of the square.
#' @param backgroundBorder,patternBorder colour for the lines in the background and foreground.
#' @param foreground,background colours for the foreground and background.
#' @param ... parameters passed to the component.
#' @details
#' Two functions that make square patterns.
#' @return return a single component
#' @family patterns
#' @seealso
#' @examples
#' print(1+1)
#' @name squarePatterns
NULL

#' @rdname squarePatterns
#' @export
squarePattern1=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x0+(x1-x0)*0.25,x0+(x1-x0)*0.75,x0+(x1-x0)*0.75,x0+(x1-x0)*0.25),c(y0+(y1-y0)*0.25,y0+(y1-y0)*0.25,y0+(y1-y0)*0.75,y0+(y1-y0)*0.75),patternBorder,foreground)
	return(answer)
}

#' @rdname squarePatterns
#' @export
squarePattern2=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c((x1+x0)*0.5,x1,(x1+x0)*0.5,x0),c(y0,(y0+y1)*0.5,y1,(y0+y1)*0.5),patternBorder,foreground)
	return(answer)
}
