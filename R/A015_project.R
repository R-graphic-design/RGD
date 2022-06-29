#' project
#'
#' Converts 3d co-ordinates into 2d co-ordinates.
#' @param x,y,z co-ordinates for points, polygons, etc
#' @param x0,x1,y0,y1,z0,z2 co-ordiantes for lines
#' @param cameraXYZ co-ordinates for the camera
#' @param thetaXYZ direction of the camera
#' @param surfaceXYZ location of the surface
#' @details
#' Uses conventions fromm physics. To Do: Add mathematics convention.
#' @return Returns a list with x and y co-ordinates.
#' @export
#' @family 3Dgraphics
#' @examples
#' print(1+1)
project=function(x,x0,x1,y,y0,y1,z,z0,z1,cameraXYZ=c(0,0,0),thetaXYZ=c(0,0,0),surfaceXYZ=c(0,0,0)){
	cosXYZ=cos(thetaXYZ)
	sinXYZ=sin(thetaXYZ)
	if(is.null(x)){
		#x0=x0-cameraXYZ[1]
		#y0=y0-cameraXYZ[2]
		#z0=z0-cameraXYZ[3]
		#x1=x1-cameraXYZ[1]
		#y1=y1-cameraXYZ[2]
		#z1=z1-cameraXYZ[3]
		#return(list(x0=x0,x1=x1,y0=y0,y1=y1))
	}else{
		x=x-cameraXYZ[1]
		y=y-cameraXYZ[2]
		z=z-cameraXYZ[3]
		dx=cosXYZ[2]*(sinXYZ[2]*y+cosXYZ[3]*x)-sinXYZ[2]*z
		dy=sinXYZ[1]*(cosXYZ[2]*z+sinXYZ[2]*(sinXYZ[3]*y+cosXYZ[3]*x))+cosXYZ[1]*(cosXYZ[3]*y-sinXYZ[3]*x)
		dz=cosXYZ[1]*(cosXYZ[2]*z+sinXYZ[2]*(sinXYZ[3]*y+cosXYZ[3]*x))+sinXYZ[1]*(cosXYZ[3]*y-sinXYZ[3]*x)
		finalX=surfaceXYZ[3]*dx/dz+surfaceXYZ[1]
		finalY=surfaceXYZ[3]*dy/dz+surfaceXYZ[2]
		return(list(x=finalX,y=finalY))
	}
}