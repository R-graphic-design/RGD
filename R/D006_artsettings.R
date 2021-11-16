#' artsettings
#'
#' @description
#' An S4 class for storing setting intended for artwork objects.
#' @slot settings a list of settings
#' @param settings a list passed to the settings slot
#' @param ... extra parameters to be added to the settings slot
#' @details
#' This class is a convenient way of adding settings to artwork objects.
#' @export
#' @examples
#' print(1+1)
#' @name artsettings
setClass("artsettings",slots=c(settings="list"))

#' @rdname artsettings
#' @method initialise artsettings
#' @export
setMethod("initialize","artsettings",function(.Object,...,settings=list()){
	.Object@settings=c(settings,list(...))
	.Object
})
#' @rdname artsettings
#' @export
settings=function(...,settings=list()){new("artsettings",...,settings=settings)}

#' @rdname artsettings
#' @export
setMethod("+",c("artwork","artsettings"),function(e1,e2){
	if("useGrid"%in%names(e2@settings)){e1@useGrid=e2@settings$useGrid}
	if("format"%in%names(e2@settings)){e1@format=e2@settings$format}
	if("folder"%in%names(e2@settings)){e1@folder=e2@settings$folder}
	if("name"%in%names(e2@settings)){e1@name=e2@settings$name}
	if("width"%in%names(e2@settings)){e1@width=e2@settings$width}
	if("height"%in%names(e2@settings)){e1@height=e2@settings$height}
	return(e1)})
	