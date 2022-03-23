#' Polar Shapes
#'
#' @description
#' Functions to draw shapes from polar co-ordinates.
#' @template param_type_polygon
#' @param x,y the center of the shape
#' @param r radius
#' @param points the number of points on single loop
#' @param loops the number of times the line wraps around the center
#' @param rotation rotation in radians. rotates the shape this much.
#' @param polarFunction if not null, gives radius instead of r
#' @param polarArguments a list of arguments passed to polarFunction
#' @details
#' makePolarShape has two modes depending on if a polar function is given instead of a radius r.
#' @examples
#' art=makeStar(r=c(0.1,NA,0.2,NA,0.4,NA),points=13,loop=6)
#' easyPlot(art,preserveAspectRatio=TRUE)
#' @name PolarShapes
NULL
#> NULL

##ADD A LOOP FEATURE AND A SKIP FEATURE TO GET OTHER SHAPES.

#' @rdname PolarShapes
#' @export
runPolarShape=function(x=0,y=0,r=c(0.2,0.1),points=5,loops=1,rotation=0,polarFunction=NULL,polarArguments=list(),repeats=1){
	if(is.null(polarFunction)){
		bearing=seq(0,2*pi,len=((length(r)*points)/loops)+1)+rotation
		bearing=rep(bearing[-length(bearing)],times=loops)
		bearing=bearing[loopSubset(!is.na(r),seq_along(bearing))]
		r=r[!is.na(r)]
		return(polar2cart(x=x,y=y,dist=rep(r,times=points),bearing=bearing))
	}else{
		bearing=seq(0,2*pi,len=(repeats*points/loops)+1)+rotation
		bearing=rep(bearing[-length(bearing)],times=loops)
		polarBearings=rep(seq(0,2*pi,len=length(bearing)/repeats),times=repeats)
		dist=do.call(polarFunction,c(list(polarBearings),polarArguments))
		return(polar2cart(x=x,y=y,dist=dist,bearing=bearing))
	}
}

#' @rdname PolarShapes
#' @export
makePolarShape=function(type="polygon",...){
	return(component(type=type,...)+action.data(fun="runPolarShape",...))
}

#' @rdname PolarShapes
#' @export
makeStar=function(...){
	return(makePolarShape(...))
}

#' @rdname PolarShapes
#' @export
makeCircle=function(r=1,points=100,...){
	return(makePolarShape(r=r,points=points,...))
}