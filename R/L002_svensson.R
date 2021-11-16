#' Johnny Svensson Attractors
#'
#' @description
#' Functions to calculate the sequence of points given by the following dynamical system in order to draw the attractor of the system.
#' * \ifelse{html}{\out{x[n+1]=d*sin(a*x[n])-sin(b*y[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#' * \ifelse{html}{\out{y[n+1]=c*cos(a*x[n])+cos(b*y[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#'
#' runSvenssonAttractor calculates the attractor. makeSvenssonAttractor makes a component that calculates and plots the Attractor.
#' @template makeTypePoints
#' @param ... parameters passed to the component and the component function runSvenssonAttractor. See "details" for more information.
#' @template attractors1
#' @param a,b,c,d The four parameters of the attractor from the above equations.
#' @template attractors2
#' @template runMake1B
#' @template runMakeXYComponent
#' @family 2dAttractors
#' @seealso \url{http://paulbourke.net/fractals/peterdejong/}
#' @examples
#' art=makeSvenssonAttractor(cex=0.1,col=rgb(0,0,0,0.1))
#' easyPlot(art,preserveAspectRatio=TRUE)
#' @name SvenssonAttractor
NULL
#> NULL

#' @rdname SvenssonAttractor
#' @export
makeSvenssonAttractor=function(type="points",...){
	return(component(type=type,...)+p2p_cf("runSvenssonAttractor",...))
}

#' @rdname SvenssonAttractor
#' @export
runSvenssonAttractor=function(a=1.4,b=1.56,c=1.4,d=-6.56,inputx=0,inputy=0,x=NULL,y=NULL,runTime=100000,sampleSize=NULL,reuseInput=FALSE){
	if(reuseInput|is.null(x)){x=inputx}
	if(reuseInput|is.null(y)){y=inputy}
	lenx=length(x)
	for(i in 1:runTime){
		x[lenx+i]=d*sin(a*x[lenx+i-1])-sin(b*y[lenx+i-1])
		y[lenx+i]=c*cos(a*x[lenx+i-1])+cos(b*y[lenx+i-1])
	}
	if(!is.null(sampleSize)){
		x=pruneStart(x,sampleSize)
		y=pruneStart(y,sampleSize)
	}
	return(list(x=x,y=y))
}