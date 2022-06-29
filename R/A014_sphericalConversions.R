#' spherical2cart
#'
#' Converts spherical co-ordinates into cartesian co-ordinates.
#' @param x,y the cartesian co-ordinates where angles are drawn from. Can be a vector and defaults to 0.
#' @param distance polar co-ordinate distance(s) from the center. Can be a vector
#' @param bearing polar co-ordinate angle(s). Can be a vector. By default this is in radians. See as.deg
#' @param as.deg defaults to false. Set to TRUE if you give bearings in degrees instead of radians. 
#' @details
#' Uses conventions fromm physics. To Do: Add mathematics convention.
#' @return Returns a list with x, y and z co-ordinates.
#' @export
#' @family sphericalConversion
#' @examples
#' print(1+1)
spherical2cart<-function(x=0,y=0,z=0,radius,inclination,azimuth,as.deg=FALSE){
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    inclination=inclination*pi/180
	azimuth=azimuth*pi/180
  }
  
  newx<-x+radius*cos(azimuth)*sin(inclination)  ##X
  newy<-y+radius*sin(azimuth)*sin(inclination)  ##Y
  newz<-z+radius*cos(inclination)
  return(list("x"=newx,"y"=newy,"z"=newz))
}
