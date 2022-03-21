#' components
#'
#' @description
#' An S4 class to represent a single element in a graphic;
#' @slot type The name(s) of the plotting function(s) used to plot the component. Defaults to "points". This can also include the names of classes linked to a plotting function.
#' @slot units A list of units to use for parameters.
#' @slot data a list of parameters.
#' @slot style a list of parameters to pass to par or gpar that are set for this component only.
#' @slot active logical. If FALSE the component functions aren't run
#' @slot visible logical. If FALSE the component isn't plotted.
#' @slot action list. Functions that act on the component or the elements of data. Changes made persist between frames. Actions run before build, camera, display and plot functions.
#' @slot build list. Functions that act on the component or the elements of data. Changes made last until the frame is plotted. Build functions run before camera, display and plot functions.
#' @slot display list. Functions that act on the component or the elements of data. Changes made last until the frame is plotted. Display functions run before plot functions. 
#' @slot frames an integerSet. The set of frames the component can be displayed in, (assuming visible is TRUE).
#' @param type,style,units,action,build,display,active,visible parameters for the constructor new("component") go directly into the relevant slots
#' @param frames If this is an integerSet, it goes directly into the relevant slot. If it is a numeric vector it is converted into an integerSet for you.
#' @param data,ref,... These parameters are merged into the data slot. Note any arguments named ".type",".frames", ".abcd", ".useData" and ".repeats" are ignored.
#' @details
#' The default component made by the constructor is a point, with no parameters, no functions and no units. It is active and plotted on every frame.
#' @export
#' @examples
#' print(1+1)
#' @name components
component <- setClass("component",
slots=c(type="character",
				     units="list",
				     data="list",
				     style="list",
					 active="logical",
					 visible="logical",
					 action="list",
					 build="list",
					 display="list",
					 frames="integerSet"))

#' @rdname components
#' @method initialise component
#' @export
setMethod("initialize","component",function(.Object,...,
		type="points",
		style=list(),
		units=list(),
		data=list(),
		action=list(),
		build=list(),
		display=list(),
		active=TRUE,
		visible=TRUE,
		ref=as.character(),
		frames=integerSet()){
	######TYPE
	.Object@type<-type
	#how to ref?
	######data
	fullList=c(data,list(...))
	fullList=fullList[!names(fullList)%in%c(".frames",".abcd",".repeats",."useData","type")]
	.Object@data<-fullList
	if(length(ref)>0){
		.Object@data<-c(.Object@data,lapply(ref,directRef))#change to mergeOverVariant??
	}
	######OTHERS
	.Object@style<-style
	.Object@units<-units
	.Object@action<-action
	.Object@build<-build
	.Object@display<-display
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