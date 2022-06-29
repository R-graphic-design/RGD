#' areas
#'
#' functions to calculate the size of a triangle
#' @param x1,x2,x3,y1,y2,y3 co-ordinates
#' @return Returns area of triangle
#' @export
#' @examples
#' areaTriangle(0.5,0,1,1,0,0)
areaTriangle=function(x1,x2,x3,y1,y2,y3){(x1*y2+x2*y3+x3*y1-x1*y3-x2*y1-x3*y2)/2}