#' Stripe Patterns
#'
#' @description
#' Functions to create striped patterns
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
#' @name stripePatterns
NULL

#' @rdname stripePatterns
#' @export
diagonalStripePattern1=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x1,x1,x0),c(y0,y1,y1),NA,foreground)
	answer=addLineToPattern(answer,x0,x1,y1,y0,patternBorder)
	return(answer)
}

#' @rdname stripePatterns
#' @export
verticalStripePattern1=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c((x1+x0)/2,x1,x1,(x1+x0)/2),c(y0,y0,y1,y1),NA,foreground)
	if(!is.na(patternBorder)){
		answer=addLineToPattern(answer,(x1+x0)/2,(x1+x0)/2,y0,y1,patternBorder)
		answer=addLineToPattern(answer,x1,x1,y0,y1,patternBorder)
		answer=addLineToPattern(answer,x0,x0,y0,y1,patternBorder)
	}	
	return(answer)
}

#' @rdname stripePatterns
#' @export
horizontalStripePattern1=function(x0,x1,y0,y1,backgroundBorder="black",background="black",patternBorder="black",foreground="white",...){
	answer=blankPattern(x0,x1,y0,y1,backgroundBorder,background,...)
	answer=addPolygonToPattern(answer,c(x0,x1,x1,x0),c((y0+y1)/2,(y0+y1)/2,y1,y1),NA,foreground)
	if(!is.na(patternBorder)){
		answer=addLineToPattern(answer,x0,x1,(y0+y1)/2,(y0+y1)/2,patternBorder)
		answer=addLineToPattern(answer,x0,x1,y0,y0,patternBorder)
		answer=addLineToPattern(answer,x0,x1,y1,y1,patternBorder)
	}	
	return(answer)
}