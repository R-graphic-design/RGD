#' Triangle Patterns
#'
#' @description
#' Functions to create triangular patterns
#' @param x0,x1,y0,y1 co-ordinates for the top, bottom, left and right of the square.
#' @param backgroundBorder,patternBorder colour for the lines in the background and foreground.
#' @param foreground,background colours for the foreground and background.
#' @param ... parameters passed to the component.
#' @details
#' @return return a single component
#' @family patterns
#' @seealso
#' @examples
#' print(1+1)
#' @name trianglePatterns
NULL

#' @rdname trianglePatterns
#' @export
trianglePattern1=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x1,x1,x0),c(y0,y1,y1),patternBorder,foreground)
	return(answer)
}


#' @rdname trianglePatterns
#' @export
trianglePattern2=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x0,x1,x1),c(y0,y0,(y0+y1)*0.5),patternBorder,foreground)
	answer=addPolygonToPattern(answer,c(x0,x1,(x0+x1)*0.5),c(y0,y1,y1),patternBorder,foreground)
	return(answer)
}


#' @rdname trianglePatterns
#' @export
trianglePattern3=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x0,x1,x1),c((y0+y1)*0.5,y0,y1),patternBorder,foreground)
	return(answer)
}