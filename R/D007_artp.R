#' artp
#'
#' @description
#' An S4 class for storing parameters intended for art objects, object functions and plotting functions.
#' @slot artp a list of parameters
#' @param artp a list passed to the p slot
#' @param ... extra parameters to be added to the p slot
#' @details
#' This class is a convenient way of adding parameters to components, layers, sections, scenes NOTYET and artwork objects.
#' @export
#' @examples
#' print(1+1)
#' @name artp
setClass("artp",slots=c(artp="list"))

#' @rdname artp
#' @method initialise artp
#' @export
setMethod("initialize","artp",function(.Object,pprefix=NULL,...,artp=list()){
	fullList=c(artp,list(...))
	fullList=fullList[!names(fullList)%in%c(".frames",".plotting",".repeats")]
	if(!is.null(pprefix)){names(fullList)=paste(pprefix,names(fullList),sep="_")}
	.Object@artp=fullList
	.Object
})
#' @rdname artp
#' @export
artp=function(...,artp=list()){new("artp",...,artp=artp)}


#' @rdname artp
#' @export
setMethod("+",c("artwork","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
	e1@p[names(e2@artp)[i]]=e2@artp[i]}
	return(e1)})
	
#' @rdname artp
#' @export
setMethod("+",c("layer","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
	e1@p[names(e2@artp)[i]]=e2@artp[i]}
	return(e1)})
#' @rdname artp
#' @export
setMethod("+",c("section","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
	e1@p[names(e2@artp)[i]]=e2@artp[i]}
	return(e1)})
#' @rdname artp
#' @export
setMethod("+",c("component","artp"),function(e1,e2){
	if(length(e2@artp)>0){
	for(i in 1:length(e2@artp)){
	e1@p[names(e2@artp)[i]]=e2@artp[i]}
	}
	return(e1)})

#' @rdname artp
#' @export
setMethod("-",c("component","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
		if(!(names(e2@artp)[i]%in%names(e1@p))){
			e1@p[names(e2@artp)[i]]=e2@artp[i]
		}
	}
	return(e1)})
#' @rdname artp
#' @export
setMethod("-",c("section","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
		if(!(names(e2@artp)[i]%in%names(e1@p))){
			e1@p[names(e2@artp)[i]]=e2@artp[i]
		}
	}
	return(e1)})
#' @rdname artp
#' @export
setMethod("-",c("layer","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
		if(!(names(e2@artp)[i]%in%names(e1@p))){
			e1@p[names(e2@artp)[i]]=e2@artp[i]
		}
	}
	return(e1)})
#' @rdname artp
#' @export
setMethod("-",c("artwork","artp"),function(e1,e2){
	for(i in 1:length(e2@artp)){
		if(!(names(e2@artp)[i]%in%names(e1@p))){
			e1@p[names(e2@artp)[i]]=e2@artp[i]
		}
	}
	return(e1)})
	