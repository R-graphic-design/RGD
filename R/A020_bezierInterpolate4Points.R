#' bezierInterpolate4Points
#'
#' Performs a bezier interpolation based on four points. Instead of requiring the middle two points, the angle of vectors from the start to point 2 and from the end to point 3 are required.
#' @param x,y co-ordinates of the start and end points.
#' @param angles the first angle is the from the start and the second is from the end.
#' @param tseq sequence of parameters for the bezier curve
#' @param tanInf value at which tan of angle is considered large enough to be infinite. I think this avoids dividing by zero? or smoother vertical lines???
#' @return Returns a curve of points. See bezier() in the bezier package for more details.
#' @export
#' @examples
#' print(1+1)
bezierInterpolate4Points=function(x,y,angles,tseq,tanInf=100){
	require(bezier)
	bezierX=0
	bezierY=0
	if(abs(tan(angles[1]))>tanInf&&abs(tan(angles[2]))>tanInf){
		bezierX=x[1]
		bezierY=(y[1]+y[2])/2
	}else if(abs(tan(angles[1]))>tanInf){
		bezierX=x[1]
		c2=y[2]-tan(angles[2])*x[2]
		bezierY=tan(angles[2])*x[1]+c2
	}else if(abs(tan(angles[2]))>tanInf){
		bezierX=x[2]
		c1=y[1]-tan(angles[1])*x[1]
		bezierY=tan(angles[1])*x[2]+c1
	}else if(angles[1]==angles[2]){
		bezierX=(x[1]+x[2])/2
		bezierY=(y[1]+y[2])/2
	}else{
		c1=y[1]-tan(angles[1])*x[1]
		c2=y[2]-tan(angles[2])*x[2]
		bezierX=(c2-c1)/(tan(angles[1])-tan(angles[2]))
	bezierY=tan(angles[1])*bezierX+c1
	}
	midX1=(bezierX+x[1])/2
	midX2=(bezierX+x[2])/2
	midY1=(bezierY+y[1])/2
	midY2=(bezierY+y[2])/2

	answer=bezier(tseq,matrix(c(x[1],midX1,bezierX,midX2,x[2],y[1],midY1,bezierY,midY2,y[2]),c(5,2)))
	return(answer)
}
