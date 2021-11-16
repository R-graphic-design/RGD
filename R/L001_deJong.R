#' Peter de Jong Attractors
#'
#' @description
#' Functions to calculate the sequence of points given by the following dynamical system in order to draw the attractor of the system.
#' * \ifelse{html}{\out{x[n+1]=sin(a*y[n])-cos(b*x[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#' * \ifelse{html}{\out{y[n+1]=sin(c*x[n])-cos(d*y[n])}}{\eqn{LATEX-LIKE CODE}{ASCII}}
#'
#' runDeJongAttractor calculates the attractor. makeDeJongAttractor makes a component that calculates and plots the Attractor.
#' @template makeTypePoints
#' @param ... parameters passed to the component and the component function runDeJongAttractor. See "details" for more information.
#' @template attractors1
#' @param a,b,c,d The four parameters of the attractor from the above equations.
#' @template attractors2
#' @template runMake1B
#' @template runMakeXYComponent
#' @family 2dAttractors
#' @seealso \url{http://paulbourke.net/fractals/peterdejong/}
#' @examples
#' art=makeDeJongAttractor(cex=0.1,col=rgb(0,0,0,0.1))
#' easyPlot(art,preserveAspectRatio=TRUE)
#' @name DeJongAttractor
NULL
#> NULL

#' @rdname DeJongAttractor
#' @export
makeDeJongAttractor=function(type="points",...){
	return(component(type=type,...)+p2p_cf("runDeJongAttractor",...))
}

#' @rdname DeJongAttractor
#' @export
runDeJongAttractor=function(a=3,b=0.4,c=2,d=0.6,inputx=0,inputy=0,x=NULL,y=NULL,runTime=100000,sampleSize=NULL,reuseInput=FALSE){
	if(reuseInput|is.null(x)){x=inputx}
	if(reuseInput|is.null(y)){y=inputy}
	lenx=length(x)
	for(i in 1:runTime){
		x[lenx+i]=sin(a*y[lenx+i-1])-cos(b*x[lenx+i-1])
		y[lenx+i]=sin(c*x[lenx+i-1])-cos(d*y[lenx+i-1])
	}
	if(!is.null(sampleSize)){
		x=pruneStart(x,sampleSize)
		y=pruneStart(y,sampleSize)
	}
	return(list(x=x,y=y))
}