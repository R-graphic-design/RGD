#' makeImage
#'
#' @description
#' Functions to make a component for the rasterImage function in the base graphics system.
#' makeImage assumes the input is an RGB array, whilst makeImageFromMatrix converts a matrix into colours for you using matrixToColours.
#' @param image a matrix or RGB array
#' @param type plotting function(s) used
#' @param xleft,xright,ytop,ybottom positioning. see rasterImage
#' @param ... parameters passed to the component or the component functions imageColourScheme and matrixToColours.
#' @return a component
#' @name makeImage
NULL
#> NULL

#' @rdname makeImage
#' @export
makeImage=function(image,type="rasterImage",xleft=0,xright=1,ytop=1,ybottom=0,...){
	return(component(image=image,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,...))
}
#' @rdname makeImage
#' @export
makeImageFromMatrix=function(...){
	return(makeImage(...)+p2p_cf("imageColourScheme",...)+c2c_cf("matrixToColours",...))
}