#' components
#'
#' @description
#' An S4 class to represent a single element in a graphic;
#' @slot type The name(s) of the plotting function(s) used to plot the component. Defaults to "points". This can also include the names of classes linked to a plotting function.
#' @slot units A list of units to use for parameters.
#' @slot p a list of parameters.
#' @slot style a list of parameters to pass to par or gpar that are set for this component only.
#' @slot active logical. If FALSE the component functions aren't run
#' @slot visible logical. If FALSE the component isn't plotted.
#' @slot functions list. Functions that act on the component or the elements of p.
#' @slot frames an integerSet. The set of frames the component can be displayed in, (assuming visible is TRUE).
#' @param type,style,units,functions,active,visible parameters for the constructor new("component") go directly into the relevant slots
#' @param frames If this is an integerSet, it goes directly into the relevant slot. If it is a numeric vector it is converted into an integerSet for you.
#' @param p,ref,... These parameters are merged into the p slot. Note any arguments named ".frames", ".plotting" and ".repeats" are ignored.
#' @details
#' The default component made by the constructor is a point, with no parameters, no functions, no units. It is active and plotted on every frame.
#' @export
#' @examples
#' print(1+1)
#' @name components
component <- setClass("component",
slots=c(type="character",
				     units="list",
				     p="list",
				     style="list",
					 active="logical",
					 visible="logical",
					 functions="list",
					 frames="integerSet"))

#' @rdname components
#' @method initialise component
#' @export
setMethod("initialize","component",function(.Object,...,
		type="points",
		style=list(),
		units=list(),
		p=list(),
		functions=list(),
		active=TRUE,
		visible=TRUE,
		ref=as.character(),
		frames=integerSet()){
	######TYPE
	.Object@type<-type
	#how to ref?
	######P
	fullList=c(p,list(...))
	fullList=fullList[!names(fullList)%in%c(".frames",".plotting",".repeats")]
	.Object@p<-fullList
	if(length(ref)>0){
		.Object@p<-c(.Object@p,lapply(ref,directRef))#change to mergeOverVariant??
	}
	######OTHERS
	.Object@style<-style
	.Object@units<-units
	.Object@functions<-functions
	.Object@visible<-visible
	.Object@active<-active
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@frames<-frames
	#######END
	.Object
})

#' @rdname components
#' @export
component=function(...){new("component",...)}