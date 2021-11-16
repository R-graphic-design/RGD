#' l2llf
#'
#' @description
#' An S4 class for storing layer functions that accept layers.
#' @slot functionList a list of functions. These can be names or actual functions.
#' @param functionList a list passed to the functionList slot
#' @param ... extra items to be added to the functionList slot
#' @details
#' This class is a convenient way of adding functions to layer objects.
#' @export
#' @examples
#' print(1+1)
#' @name l2llf
setClass("l2llf",slots=c(functionList="ANY"))

#' @rdname l2llf
#' @method initialise l2llf
#' @export
setMethod("initialize","l2llf",function(.Object,...,functionList=as.character()){
	.Object@functionList=c(functionList,...)
	.Object
})

#' @rdname l2llf
#' @export
l2l_lf=function(functions=c(),...){
	answer=list()
	.frames=list(...)$.frames
	.plotting=list(...)$.plotting
	if(is.null(.frames)){.frames=integerSet()}
	if(class(.frames)=="list"){
		for(i in seq_along(.frames)){
			if(class(.frames[[i]])=="numeric"){.frames[[i]]=frames(.frames[[i]])}
		}
	}
	if(class(.frames)=="numeric"){
		.frames=list(frames(.frames))
	}
	if(class(.frames)=="integerSet"){
		.frames=list(.frames)
	}
	if(is.null(.plotting)){.plotting=FALSE}
	if(class(functions)=="list"){
		for(i in seq_along(functions)){
			answer[[i]]=layerfunction(functions[[i]],returnLayer=TRUE,frames=.frames[[loopIndex(length(.frames),i)]],plotting=loopSubset(.plotting,i))
		}
	}else if(class(functions)=="character"){
		for(i in seq_along(functions)){
			answer[[i]]=layerfunction(functions[i],returnLayer=TRUE,frames=.frames[[loopIndex(length(.frames),i)]],plotting=loopSubset(.plotting,i))
		}		
	}
	return(new("l2llf",answer))
}

#' @rdname l2llf
#' @export
setMethod("+",c("layer","l2llf"),function(e1,e2){
	e1@functions=c(e1@functions,e2@functionList)
	return(e1)})

#' @rdname l2llf
#' @export
setMethod("-",c("layer","l2llf"),function(e1,e2){
	e1@functions=c(e2@functionList,e1@functions)
	return(e1)})

