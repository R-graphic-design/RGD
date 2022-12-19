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
project=function(x,x0,x1,y,y0,y1,z,z0,z1,cameraXYZ=c(0,0,0),thetaXYZ=c(0,0,0),surfaceXYZ=c(0,0,0),prepareBackCull=TRUE){
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
		sidesToShow=c()
		x=x-cameraXYZ[1]
		y=y-cameraXYZ[2]
		z=z-cameraXYZ[3]
		if(prepareBackCull){
			x=c(NA,x)
			y=c(NA,y)
			z=c(NA,z)
			faceIndex=cumsum(is.na(x))
			for(i in 1:max(faceIndex)){
				p1=c(x[faceIndex==i][2],y[faceIndex==i][2],z[faceIndex==i][2])
				p2=c(x[faceIndex==i][3],y[faceIndex==i][3],z[faceIndex==i][3])
				p3=c(x[faceIndex==i][4],y[faceIndex==i][4],z[faceIndex==i][4])
				edge1=p3-p1
				edge2=p3-p2
				faceNormal=c(edge1[2]*edge2[3]-edge1[3]*edge2[2],edge1[3]*edge2[1]-edge1[1]*edge2[3],edge1[1]*edge2[2]-edge1[2]*edge2[1])
				finalAngle=faceNormal[1]*p1[1]+faceNormal[2]*p1[2]+faceNormal[3]*p1[3]
				if(finalAngle<0){
					sidesToShow=c(sidesToShow,i)
				}
			}
			x=x[-1]
			y=y[-1]
			z=z[-1]
		}
		dx=cosXYZ[2]*(sinXYZ[2]*y+cosXYZ[3]*x)-sinXYZ[2]*z
		dy=sinXYZ[1]*(cosXYZ[2]*z+sinXYZ[2]*(sinXYZ[3]*y+cosXYZ[3]*x))+cosXYZ[1]*(cosXYZ[3]*y-sinXYZ[3]*x)
		dz=cosXYZ[1]*(cosXYZ[2]*z+sinXYZ[2]*(sinXYZ[3]*y+cosXYZ[3]*x))+sinXYZ[1]*(cosXYZ[3]*y-sinXYZ[3]*x)
		finalX=surfaceXYZ[3]*dx/dz+surfaceXYZ[1]
		finalY=surfaceXYZ[3]*dy/dz+surfaceXYZ[2]
		if(prepareBackCull){
			return(list(x=finalX,y=finalY,sidesToShow=sidesToShow))
		}else{
			return(list(x=finalX,y=finalY))
		}
	}
}