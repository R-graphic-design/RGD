#' Clifford Attractors
#'
#' @description
#' Functions to calculate the sequence of points given by the following dynamical system in order to draw the attractor of the system.
#' * \ifelse{html}{\out{x[n+1]=d*sin(a*x[n])-sin(b*y[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#' * \ifelse{html}{\out{y[n+1]=c*cos(a*x[n])+cos(b*y[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#'
#' runCliffordAttractor calculates the attractor. makeCliffordAttractor makes a component that calculates and plots the Attractor.
#' @template param_type_points
#' @param ... parameters passed to the component and the component function runCliffordAttractor. See "details" for more information.
#' @param a,b,c,d The four parameters of the attractor from the above equations.
#' @family 2dAttractors
#' @seealso \url{http://paulbourke.net/fractals/peterdejong/}
#' @examples
#' art=makeCliffordAttractor(cex=0.1,col=rgb(0,0,0,0.1))
#' easyPlot(art,preserveAspectRatio=TRUE)
#' @name CliffordAttractor
NULL
#> NULL

#' @rdname CliffordAttractor
#' @export
makeCliffordAttractor=function(type="points",...){
	return(component(type=type,...)+action.data(fun="runCliffordAttractor",...))
}

#' @rdname CliffordAttractor
#' @export
runCliffordAttractor=function(a=-1.4,b=1.6,c=1,d=0.7,inputx=0,inputy=0,x=NULL,y=NULL,runTime=100000,sampleSize=NULL,reuseInput=FALSE){
	if(reuseInput|is.null(x)){x=inputx}
	if(reuseInput|is.null(y)){y=inputy}
	lenx=length(x)
	for(i in 1:runTime){
		x[lenx+i]=sin(a*y[lenx+i-1])+c*cos(a*x[lenx+i-1])
		y[lenx+i]=sin(b*x[lenx+i-1])+d*cos(b*y[lenx+i-1])
	}
	if(!is.null(sampleSize)){
		x=pruneStart(x,sampleSize)
		y=pruneStart(y,sampleSize)
	}
	return(list(x=x,y=y))
}