#' layers
#'
#' @description
#' An S4 class to represent a collection of single elements in a graphic;
#' @slot components A list of components contained in the layer
#' @slot layers A list of layers contained in the layer
#' @slot units A list of units inherited by components and layers in this layer
#' @slot data a list of parameters inherited by components and layers in this layer
#' @slot style a list of parameters inherited by components and layers in this layer
#' @slot visible logical. If FALSE the layer isn't plotted.
#' @slot action list. Functions that act on the layer or its data or its components.
#' @slot build list.
#' @slot display list.
#' @slot frames an integerSet. The set of frames the layer can be displayed in, (assuming visible is TRUE).
#' @param components,... These parameters are merged into the components slot.
#' @param layers,style,units,functions,visible parameters for the constructor new("layer") go directly into the relevant slots
#' @param frames If this is an integerSet, it goes directly into the relevant slot. If it is a numeric vector it is converted into an integerSet for you.
#' @details
#' The default layer made by the constructor completely empty, with no parameters, no components, no sublayers, no functions, no units. It is active and plotted on every frame.
#' @export
#' @examples
#' print(1+1)
#' @name layers
layer <- setClass("layer",
	slots=c(components="list",
				 layers="list",
				 visible="logical",
				 style="list",
				 data="list",
				 units="list",
				 action="list",
				 build="list",
				 display="list",
				 frames="integerSet"))
				 
#' @rdname layers
#' @method initialise layer
#' @export
setMethod("initialize","layer",function(.Object,...,
		style=list(),
		visible=TRUE,
		units=list(),
		data=list(),
		action=list(),
		build=list(),
		display=list(),
		components=list(),
		layers=list(),
		frames=integerSet()){
	.Object@components<-c(components,list(...))
	.Object@layers<-layers
	.Object@visible<-visible
	.Object@data<-data
	.Object@style<-style
	.Object@units<-units
	.Object@action<-action
	.Object@build<-build
	.Object@display<-display
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@frames=frames
	.Object
})
#' @rdname layers
#' @export
setMethod("+",c("layer","component"),function(e1,e2){
e1@components=c(e1@components,e2)
return(e1)
})
#' @rdname layers
#' @export
setMethod("-",c("layer","component"),function(e1,e2){
e1@components=c(e2,e1@components)
return(e1)
})

#' @rdname layers
#' @export
layer=function(...){new("layer",...)}