#' makeImage
#'
#' @description
#' Functions to make a component for the rasterImage function in the base graphics system.
#' makeImage assumes the input "image"is an RGB array, whilst makeImageFromMatrix converts a matrix into colours for you using matrixToColours.
#' @param type plotting function(s) used
#' @param xleft,xright,ytop,ybottom positioning. see rasterImage
#' @param ... parameters passed to the component or the component functions imageColourScheme and matrixToColours.
#' @return a component
#' @name makeImage
NULL
#> NULL

#' @rdname makeImage
#' @export
makeImage=function(type="rasterImage",xleft=0,xright=1,ytop=1,ybottom=0,...){
	return(component(type=type,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom,...))
}

#' @rdname makeImage
#' @export
makeImageFromMatrix=function(...){
	return(makeImage(...)+action.data("matrixToColours",...))
}

#' @rdname makeImage
#' @export
makeImageFromMatrix2=function(.useData=c(FALSE,TRUE),...){
	return(makeImage(...)+functionList(fun=c("imageColourScheme","matrixToColours"),.useData=.useData,...))
}

