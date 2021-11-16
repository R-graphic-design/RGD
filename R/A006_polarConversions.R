#' polar2cart
#'
#' Converts polar co-ordinates into cartesian co-ordinates.
#' @param x,y the cartesian co-ordinates where angles are drawn from. Can be a vector and defaults to 0.
#' @param distance polar co-ordinate distance(s) from the center. Can be a vector
#' @param bearing polar co-ordinate angle(s). Can be a vector. By default this is in radians. See as.deg
#' @param as.deg defaults to false. Set to TRUE if you give bearings in degrees instead of radians. 
#' @return Returns a list with x and y co-ordinates.
#' @export
#' @family polarConversion
#' @examples
#' polar2cart(0,0,1,c(0,pi/2,pi,3*pi/2))

polar2cart<-function(x=0,y=0,distance,bearing,as.deg=FALSE){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    bearing=bearing*pi/180
  }
  
  newx<-x+distance*cos(bearing)  ##X
  newy<-y+distance*sin(bearing)  ##Y
  return(list("x"=newx,"y"=newy))
}
