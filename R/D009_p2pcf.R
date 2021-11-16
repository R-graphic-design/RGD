#' p2pcf
#'
#' @description
#' An S4 class for storing component functions that accept parameters.
#' @slot functionList a list of functions. These can be names or actual functions.
#' @param functionList a list passed to the functionList slot
#' @param ... extra items to be added to the functionList slot
#' @details
#' This class is a convenient way of adding functions to component objects.
#' @export
#' @examples
#' print(1+1)
#' @name p2pcf
setClass("p2pcf",slots=c(functionList="ANY"))

#' @rdname p2pcf
#' @method initialise p2pcf
#' @export
setMethod("initialize","p2pcf",function(.Object,functionList=as.character()){
	.Object@functionList=c(functionList)
	.Object
})


#' @rdname p2pcf
#' @export
p2p_cf=function(functions=c(),...){
	answer=list()
	.frames=list(...)$.frames
	.plotting=list(...)$.plotting
	.repeats=list(...)$.repeats
	if(is.null(.repeats)){.repeats=1}
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
			answer[[i]]=componentfunction(functions[[i]],returnComponents=FALSE,frames=.frames[[loopIndex(length(.frames),i)]],plotting=loopSubset(.plotting,i),repeats=loopSubset(.repeats,i))
		}
	}else if(class(functions)=="character"){
		for(i in seq_along(functions)){
			answer[[i]]=componentfunction(functions[i],returnComponents=FALSE,frames=.frames[[loopIndex(length(.frames),i)]],plotting=loopSubset(.plotting,i),repeats=loopSubset(.repeats,i))
		}		
	}
	return(new("p2pcf",answer))
}

#' @rdname p2pcf
#' @export
setMethod("+",c("component","p2pcf"),function(e1,e2){
	e1@functions=c(e1@functions,e2@functionList)
	return(e1)})

#' @rdname p2pcf
#' @export
setMethod("-",c("component","p2pcf"),function(e1,e2){
	e1@functions=c(e2@functionList,e1@functions)
	return(e1)})
