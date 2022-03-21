#' pages
#'
#' @description
#' An S4 class to collect sections. It can represent a page in a pdf.;
#' @slot sections A list of sections contained in the page
#' @slot name A name for the file if the artwork is plotted in mode 2
#' @slot deviceSettings A list of other device settings not covered by existing slots.
#' @slot style A list of parameters inherited by all sections, layers and components in the page
#' @slot data A list of parameters inherited by all sections, layers and components in the page
#' @slot units A list of units inherited by all sections, layers and components in the page
#' @slot frames numeric vector. The set of frames
#' @slot framesToPlot numeric vector. The set of frames plotting functions can be run.
#' @param sections,... These parameters are merged into the sections slot.
#' @param deviceSettings, name, style, data, units, frames, framesToPlot parameters for the constructor new("page") go directly into the relevant slots
#' @details
#' The default page...
#' @export
#' @examples
#' print(1+1)
#' @name pages
setClass("page",slots=c(sections="list",
				name="character",
				style="list",
				data="list",
				units="list",
				deviceSettings="list",
				frames="numeric",
				framesToPlot="numeric"))#CHANGE THIS TO INTEGER SET FOR BETTER framesToPlot???

#' @method initialise pages
#' @export
setMethod("initialize","page",function(.Object,..., 
		sections=list(),
		deviceSettings=list(),
		name="page",
		style=list(),
		data=list(),
		units=list(),
		frames=1,
		framesToPlot=1){
			.Object@deviceSettings<-deviceSettings
			.Object@sections <- c(sections,list(...))
			.Object@name <- name
			.Object@style <- style
			.Object@data <- data
			.Object@units <- units
			.Object@frames<-frames
			.Object@framesToPlot<-framesToPlot
			.Object
		})
		
#' @rdname pages
#' @export
page.new=function(...){new("page",...)}