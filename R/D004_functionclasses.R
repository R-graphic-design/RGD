#' artclasses
#'
#' @description
#' An S4 class for storing class-function associations.
#' @slot classList a list of class-function associations
#' @param classList a list passed to the classList slot
#' @param ... extra items to be added to the classList slot
#' @details
#' This class is a convenient way of adding classes to artwork objects.
#' @export
#' @examples
#' print(1+1)
#' @name artclasses
setClass("artclasses",slots=c(classList="character"))

#' @rdname artclasses
#' @method initialise artclasses
#' @export
setMethod("initialize","artclasses",function(.Object,...,classList=as.character()){
	.Object@classList=c(classList,...)
	.Object
})
#' @rdname artclasses
#' @export
artClasses=function(...){new("artclasses",...)}
#' @rdname artclasses
#' @export
#The + adds classes and overrides if there is a conflict.
setMethod("+",c("artwork","artclasses"),function(e1,e2){
	for(i in 1:length(e2@classList)){
	e1@classes[names(e2@classList)[i]]=e2@classList[i]}
	return(e1)})

#' @rdname artclasses
#' @export
setMethod("-",c("artwork","artclasses"),function(e1,e2){
	for(i in 1:length(e2@classList)){
		if(!(names(e2@classList)[i]%in%names(e1@classes))){
			e1@classes[names(e2@classList)[i]]=e2@classList[i]
		}
	}
	return(e1)})