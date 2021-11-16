#' artstyle
#'
#' @description
#' An S4 class for storing parameters intended for par and gpar.
#' @slot artstyle a list of parameters
#' @param artstyle a list passed to the artstyle slot
#' @param ... extra parameters to be added to the artstyle slot
#' @details
#' This class is a convenient way of adding styles to components, layers, sections, scenes NOTYET and artwork objects.
#' @export
#' @examples
#' print(1+1)
#' @name artstyle
setClass("artstyle",slots=c(artstyle="list"))

#' @rdname artstyle
#' @method initialise artstyle
#' @export
setMethod("initialize","artstyle",function(.Object,...,artstyle=list()){
	.Object@artstyle=c(artstyle,list(...))
	.Object
})
#' @rdname artstyle
#' @export
artstyle=function(...){new("artstyle",...)}


##NO SCENE???
#The + adds styles, and overrides if there is a conflict.
#' @rdname artstyle
#' @export
setMethod("+",c("artwork","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
	e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("+",c("section","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
	e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("+",c("layer","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
	e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("+",c("component","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
	e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("-",c("artwork","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
		if(!(names(e2@artstyle)[i]%in%names(e1@style))){
			e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]
		}
	}
	return(e1)})
#The - adds styles, but doesn't override if there is a conflict.
#useful for setting defaults AFTER set up in easyplot
#' @rdname artstyle
#' @export
setMethod("-",c("section","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
		if(!(names(e2@artstyle)[i]%in%names(e1@style))){
			e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]
		}
	}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("-",c("layer","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
		if(!(names(e2@artstyle)[i]%in%names(e1@style))){
			e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]
		}
	}
	return(e1)})
#' @rdname artstyle
#' @export
setMethod("-",c("component","artstyle"),function(e1,e2){
	for(i in 1:length(e2@artstyle)){
		if(!(names(e2@artstyle)[i]%in%names(e1@style))){
			e1@style[names(e2@artstyle)[i]]=e2@artstyle[i]
		}
	}
	return(e1)})