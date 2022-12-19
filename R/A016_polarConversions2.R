#' cart2polar
#'
#' Converts polar co-ordinates into cartesian co-ordinates.
#' @param x,y,x0,y0,x1,y1 numerical. See details.
#' @param lineSegments logical. See details.
#' @param forcePositive defaults to FALSE. Setting this to true ensures the bearings are between 0 and 2*pi instead of from -pi to pi.
#' @details
#' If none of the below apply (the default), the input vector is (x,y). i.e. from the origin to x,y. 
#'
#' x and y are ignored if all of x0,y0,x1,y1 are provided and the input vectors are (x1-x0,y1-y0). i.e. from (x0,y0) to (x1,y1). x1 and y1 are ignored otherwise.
#'
#' if all of x0,y0,x1,y1 are NULL, only x and y are used. If lineSegments is FALSE, the default behaviour applies. If TRUE, the vectors are along the line given by x and y, from the first point to the second and so on.
#' @return Returns a list with r and bearing co-ordinates.
#' @export
#' @family polarConversion
#' @examples
#' print(1+1)

#' @name cart2polar
#' @export
cart2polar<-function(x=0,y=0,x0=NULL,y0=NULL,x1=NULL,y1=NULL,lineSegments=FALSE,forcePositive=FALSE){
	xx=0
	yy=0
	if(is.null(y1)&&is.null(x1)&&is.null(x0)&&is.null(y0)&&lineSegments&&length(x)>1&&length(y)>1){
		xx=x[-1]-x[-length(x)]
		yy=y[-1]-y[-length(y)]	
	}else if(!is.null(y1)&&!is.null(x1)&&!is.null(x0)&&!is.null(y0)){
		xx=x1-x0
		yy=y1-y0
	}else{
		xx=x
		yy=y
	}
	
	newR=sqrt(xx^2+yy^2)
	newBearing=acos(xx/newR)
	if(forcePositive){
		newBearing[yy<0]=2*pi-newBearing[yy<0]
	}else{
		newBearing[yy<0]=-newBearing[yy<0]
	}
	return(list(r=newR,bearing=newBearing))
}