#' Peter de Jong Attractors
#'
#' runDeJongAttractor calculates the attractor. makeDeJongAttractor makes a component that calculates and plots the Attractor.
#' @template makeType
#' @param ... parameters passed to the component and the component function runDeJongAttractor. See "details" for more information.
#' @param inputx,inputy	x and y co-ordinates to be used as initial conditions. Both default to 0. This is the default way to give initial conditions. See x,y and reuseInput.
#' @param x,y x and y co-ordinates. Can be vectors of the same length. If they are, the final value will represent the current position. Defaults to NULL, which forces the use of inputx and input y. If both are given, reuseInput determines the initial conditions. Using x and y as initial conditions is useful for creating animations where you add more points to the attractor with each frame.
#' @param runTime The number of iterations to calculate. Each iteration is added to the end of x and y. Defaults to 100000. sampleSize can be used to trim this down.
#' @param a,b,c,d The four parameters of the Peter de Jong attractor.
#' @param sampleSize If a number, x and y are trimmed to this length before being returned using pruneStart. Defaults to NULL which means x and y aren't trimmed.
#' @param reuseInput Forces the use of inputx and inputy as initial values. Defaults to FALSE. Setting this to TRUE is useful for creating animations where the parameters a,b,c and d are varied.
#' @details style, units, p, functions, active, visible, and frames are passed to the component slots of the same name.
#'		
#' ref is a vector of data references that are added to the p slot in the component
#'
#' .frames, .plotting and .repeats control runDeJongAttractor
#' 
#' All other parameters are added to the p slot in the component. This can include graphical parameters for the plotting function(s) or runDeJongAttractor.
#'	
#' @return Returns a component with runDeJongAttractor as a commponent function 
#' @examples
#' art=makeDeJongAttractor
#' easyPlot(art)
#'
#' art=makeDeJongAttractor()
#' easyPlot(art)
#' @name DeJongAttractor
NULL
#> NULL

#' @rdname DeJongAttractor
#' @export
makeDeJongAttractor=function(type="points",...){
	return(new("component",type=type,...)+p2p_cf("runDeJongAttractor,..."))
}

#' @rdname DeJongAttractor
#' @export
runDeJongAttractor=function(inputx=0,inputy=0,x=NULL,y=NULL,runTime=100000,a=3,b=0.4,c=2,d=0.6,sampleSize=NULL,reuseInput=FALSE){
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