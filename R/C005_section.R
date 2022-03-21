#' sections
#'
#' @description
#' An S4 class to represent a piece of a graphic. It can include a co-ordinate system.
#' @slot x,y,just used to position the section
#' @slot width,height used to give the size of the section
#' @slot clip if grid is used, this controls clipping.
#' @slot data a list of parameters inherited by components, layers and sections in this section
#' @slot style a list of parameters inherited by components, layers and sections in this section
#' @slot angle used for rotation of viewports in grid
#' @slot xscale,yscale used for co-ordinates that use "native" co-ordinate units
#' @slot layers a list of layers
#' @slot sections a list of sections
#' @slot units a list of units inherited by components, layers and sections in this section
#' @slot action,build,display a list of section functions that can apply to the section, the list of parameters in data or layers, components and sections contained in the section.
#' @slot camera a list of functions that apply to layers and components in the section.
#' @slot tags USED??? REMOVE??
#' @slot frames controls which frames the section is plotted.
#' @param layers,... These parameters are merged into the layers slot.
#' @param x,y,width,height,clip,angle,xscale,yscale,sections,p,style,units,functions,tags parameters for the constructor new("section") go directly into the relevant slots
#' @param just can be 1 or 2 numbers, or a character vector including "left", "right", "top" or "bottom".
#' @param tableBox USED??????? REMOVE????????
#' @param borderCol,fillCol,borderLWD REALLY NOT USED??? REMOVE
#' @param frames If this is an integerSet, it goes directly into the relevant slot. If it is a numeric vector it is converted into an integerSet for you.
#' @details
#' The default layer made by the constructor completely empty, with no parameters, no components, no sublayers, no functions, no units. It is active and plotted on every frame.
#' @export
#' @examples
#' print(1+1)
#' @name sections
setClass("section",slots=c(x="ANY",#should be unit or unit.arithmetic OR LIST for clever positioning(BUT IN addBorderBASE they are just numeric...ADD vpUNITS?)
				y="ANY",#should be unit or unit.arithmetic
				width="ANY",#should be unit or unit.arithmetic
				height="ANY",#should be unit or unit.arithmetic
				just="numeric",
				clip="ANY",
				xscale="numeric",
				yscale="numeric",
				angle="numeric",
				layers="list",
				sections="list",
				data="list",
				style="list",
				units="list",
				action="list",
				build="list",
				camera="list",
				display="list",
				tags="character",
				frames="integerSet"))
				
#' @rdname sections
#' @method initialise section
#' @export
setMethod("initialize","section",function(.Object,...,
		x=unit(0.5,"npc"),
		y=unit(0.5,"npc"),
		width=unit(1,"npc"),
		height=unit(1,"npc"),
		just="centre",
		clip="inherit",
		xscale=0:1,
		yscale=0:1,
		angle=1,
		layers=list(),
		sections=list(),
		data=list(),
		style=list(),
		units=list(),
		borderCol=NULL,# border items not yet used
		fillCol=NULL,
		borderLWD=0.05,
		tableBox=NULL,
		action=list(),
		build=list(),
		camera=list(),
		display=list(),
		frames=integerSet(),
		tags=as.character()){#tableBox implemented but could change??
	.Object@x<-x
	.Object@y<-y
	.Object@width<-width
	.Object@height<-height
	#see below for just (you can add "centre", c("top",left") etc
	.Object@clip<-clip
	.Object@xscale<-xscale
	.Object@yscale<-yscale
	.Object@angle<-angle	
	.Object@layers<-c(layers,list(...))
	.Object@sections<-sections
	#.Object@visible<-visible
	.Object@data<-data
	.Object@style<-style
	.Object@units<-units
	.Object@action<-action
	.Object@build<-build
	.Object@display<-display
	.Object@camera<-camera
	.Object@tags<-tags
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@frames<-frames
	#JUST FLEXIBLE INPUT
	if(is.numeric(just)){
		if(length(just)==1){
		.Object@just=c(just,just)
		}else{
		.Object@just=just
		}
	}else{
		adjustx=0.5
		adjusty=0.5
		if("left"%in%.Object@just){adjustx=0}
		if("right"%in%.Object@just){adjustx=1}
		if("top"%in%.Object@just){adjusty=0}
		if("bottom"%in%.Object@just){adjusty=1}
		.Object@just=c(adjustx,adjusty)
	}
	#FLEXIBLE x/y/width/height from tableBox
	#NOT SURE HOW IT WORKS....
	#replace with row... column?
	if(length(tableBox)>0){
		if("x"%in%names(tableBox)){
			.Object@just[1]=1
			.Object@width=unit(1/tableBox$x[2],"npc")
			.Object@x=unit(tableBox$x[1]/tableBox$x[2],"npc")
		}
		if("y"%in%names(tableBox)){
			.Object@just[2]=1
			.Object@height=unit(1/tableBox$y[2],"npc")
			.Object@y=unit(tableBox$y[1]/tableBox$y[2],"npc")
		}
	}
	#If you give border information, you get border/fill for the viewport.
	#need to understand creating/plotting rectangles to do this...
	if((is.null(borderCol)+is.null(fillCol))<2){
		#tempBC=borderCol
		#if(is.null(borderCol)){tempBC="black"}
		#tempFC=fillCol
		#if(is.null(fillCol)){tempFC=0}
		#tempComponent=new("component",type="rect",style=list(border=tempBC,fill=tempFC),x=0.5,y=0.5,width=1,height=1,just=c(0.5,0.5),lwd=borderLWD)
		#newLayer=new("layer",list(tempComponent))
		#.Object@layers=c(newLayer,.Object@layers)
		#if(length(.Object@layersToPlot)>0){
		#	.Object@layersToPlot=c(1,.Object@layersToPlot+1)
		#}
	}
	.Object
})

#' @rdname sections
#' @export
section=function(...){new("section",...)}