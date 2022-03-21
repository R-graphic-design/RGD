#' artdata
#'
#' @description
#' An S4 class for storing parameters intended for art objects, object functions and plotting functions.
#' @slot artdata a list of parameters
#' @param artdata a list passed to the data slot
#' @param ... extra parameters to be added to the data slot
#' @details
#' This class is a convenient way of adding parameters to components, layers, sections, scenes NOTYET and artwork objects.
#' @export
#' @examples
#' print(1+1)
#' @name artdata
setClass("artdata",slots=c(artdata="list"))

#' @rdname artdata
#' @method initialise artdata
#' @export
setMethod("initialize","artdata",function(.Object,pprefix=NULL,...,artdata=list()){
	fullList=c(artdata,list(...))
	fullList=fullList[!names(fullList)%in%c(".frames",".plotting",".repeats")]
	if(!is.null(pprefix)){names(fullList)=paste(pprefix,names(fullList),sep="_")}
	.Object@artdata=fullList
	.Object
})
#' @rdname artdata
#' @export
artdata=function(...,artdata=list()){new("artdata",...,artdata=artdata)}


#' @rdname artdata
#' @export
setMethod("+",c("artwork","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
	e1@data[names(e2@artdata)[i]]=e2@artdata[i]}
	return(e1)})
	
#' @rdname artdata
#' @export
setMethod("+",c("layer","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
	e1@data[names(e2@artdata)[i]]=e2@artdata[i]}
	return(e1)})
#' @rdname artdata
#' @export
setMethod("+",c("section","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
	e1@data[names(e2@artdata)[i]]=e2@artdata[i]}
	return(e1)})
#' @rdname artdata
#' @export
setMethod("+",c("component","artdata"),function(e1,e2){
	if(length(e2@artdata)>0){
	for(i in 1:length(e2@artdata)){
	e1@data[names(e2@artdata)[i]]=e2@artdata[i]}
	}
	return(e1)})

#' @rdname artdata
#' @export
setMethod("-",c("component","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
		if(!(names(e2@artdata)[i]%in%names(e1@data))){
			e1@data[names(e2@artdata)[i]]=e2@artdata[i]
		}
	}
	return(e1)})
#' @rdname artdata
#' @export
setMethod("-",c("section","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
		if(!(names(e2@artdata)[i]%in%names(e1@data))){
			e1@data[names(e2@artdata)[i]]=e2@artdata[i]
		}
	}
	return(e1)})
#' @rdname artdata
#' @export
setMethod("-",c("layer","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
		if(!(names(e2@artdata)[i]%in%names(e1@data))){
			e1@data[names(e2@artdata)[i]]=e2@artdata[i]
		}
	}
	return(e1)})
#' @rdname artdata
#' @export
setMethod("-",c("artwork","artdata"),function(e1,e2){
	for(i in 1:length(e2@artdata)){
		if(!(names(e2@artdata)[i]%in%names(e1@data))){
			e1@data[names(e2@artdata)[i]]=e2@artdata[i]
		}
	}
	return(e1)})
	