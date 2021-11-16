#' artwork
#'
#' @description
#' An S4 class to represent an entire graphic, artwork or animation;
#' @slot pages A list of pages contained in the layer
#' @slot name A name for the file if saved to a single file
#' @slot format Can be "screen", "pdf", "jpg", "bitmap", "cairoPDF", "png" or "cairoPNG". 
#' @slot deviceSettings A list of other device settings not covered by existing slots.
#' @slot width width of the artwork
#' @slot height height of the artwork
#' @slot mode controls the meaning of frames and how animations are handled. See details.
#' @slot folder location for the artwork to be saved if saved to a file
#' @slot frameFolder location for frames to be saved if doing animations with mode 3
#' @slot useGrid logical. if TRUE grid graphics are used instead of base graphics
#' @slot useShowText logical to use showText in devices that support it.
#' @slot viewports WHAT IS THIS?? DELETE????
#' @slot fonts a character vector of font names.
#' @slot style A list of parameters inherited by all pages, sections, layers and components in the artwork
#' @slot p A list of parameters inherited by all pages, sections, layers and components in the artwork
#' @slot units A list of units inherited by all pages, sections, layers and components in the artwork
#' @slot classes A list that connects component types to plotting functions.
#' @slot psdata NOT USED ANYWHERE WHAT IS THIS DELETE IT?????????
#' @slot framerate A number used for animations (mode 3)
#' @param pages,... These parameters are merged into the pages slot.
#' @param name,format,deviceSettings,width,height,mode,folder,frameFolder,useGrid,useShowText,viewports,fonts,style,p,units,classes,psdata,framerate parameters for the constructor new("artwork") go directly into the relevant slots
#' @details
#' The default artwork is a single frame image, displayed on screen with no margins, and no content.
#' 
#' The possible values of mode are:
#'
#' * mode=-2 All functions like component functions, layer functions etc are run and the resulting artwork is returned.
#'
#' * mode=-1 As mode -2, but only the maximal co-ordinates for xlim and ylim are returned.
#'
#' * mode=0 All frames are plotting on a single device, on page 1 of the file where appropriate
#'
#' * mode=1 On devices with multiple pages, seperate frames are on seperate pages.
#'
#' * mode=2 Seperate frames are saved in seperate files
#'
#' * mode=3 As mode 2, and ffmpeg is used to create a video afterwards.
#' @details2 If you do artwork +or- a list, it will +or- each element in the list sequentially. 
#' @import grid
#' @export
#' @examples
#' print(1+1)
#' @name artwork
setClass("artwork",slots=c(pages="list",
				name="character",
				format="character",
				deviceSettings="list",
				width="numeric",
				height="numeric",
				mode="numeric",
				folder="character",
				frameFolder="character",
				useGrid="logical",
				useShowText="logical",##? Alternative text in pdfs...
				viewports="list",
				fonts="ANY",
				style="list",
				p="list",
				units="list",
				classes="character",
				psdata="list",
				framerate="numeric"))
#' @rdname artwork
#' @method initialise artwork
#' @export
setMethod("initialize","artwork",function(.Object,..., 
		name="artwork",
		pages=list(),
		deviceSettings=list(),
		format="screen",
		folder="",
		frameFolder="",
		width=4,
		height=4,
		mode=0,
		useGrid=FALSE,
		useShowText=FALSE,
		viewports=list(),
		style=list(mai=c(0,0,0,0),xaxt="n",yaxt="n",yaxs="i",xaxs="i",mgp=c(0,0,0),oma=c(0,0,0,0),plt=c(0,1,0,1)),
		p=list(artBorder_col="white",artBorder_border="black"),
		units=list(),
		fonts="",
		classes=c(artBorder="rect"),
		psdata=list(),
		framerate=10){
              .Object@pages <- c(pages,list(...))
		.Object@useGrid<-useGrid
		.Object@useShowText<-useShowText
		.Object@psdata<-psdata
		.Object@name<-name
		.Object@mode<-mode
		.Object@folder<-folder
		.Object@frameFolder<-frameFolder
		.Object@viewports<-viewports
.Object@format <- format
.Object@deviceSettings<-deviceSettings
.Object@width <- width
.Object@height <- height
.Object@fonts<-fonts
.Object@style<-style
.Object@units<-units
.Object@p<-p
.Object@classes<-classes
.Object@psdata<-psdata
.Object@framerate<-framerate
		 .Object
            }
            
)
#' @rdname artwork
#' @export
setMethod("+",c("artwork","list"),function(e1,e2){
for(i in 1:length(e2)){e1=e1+e2[[i]]}
return(e1)
})
#' @rdname artwork
#' @export
setMethod("-",c("artwork","list"),function(e1,e2){
for(i in 1:length(e2)){e1=e1-e2[[i]]}
return(e1)
})

#' @rdname artwork
#' @export
artwork=function(...){new("artwork",...)}