#' quickfunctions
#'
#' @description
#' An S4 class for storing layer functions that accept layers.
#' @slot functionList a list of functions. These can be names or actual functions.
#' @param functionList a list passed to the functionList slot
#' @param ... extra items to be added to the functionList slot
#' @details
#' This class is a convenient way of adding functions to layer objects.
#' @examples
#' print(1+1)
#' @name quickfunctions
NULL

#' @rdname quickfunctions
#' @export
action.data=function(fun=c(),.abcd="a",.useData=TRUE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
build.data=function(fun=c(),.abcd="b",.useData=TRUE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
camera.data=function(fun=c(),.abcd="c",.useData=TRUE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
display.data=function(fun=c(),.abcd="d",.useData=TRUE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
action.object=function(fun=c(),.abcd="a",.useData=FALSE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
build.object=function(fun=c(),.abcd="b",.useData=FALSE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
camera.object=function(fun=c(),.abcd="c",.useData=FALSE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}

#' @rdname quickfunctions
#' @export
display.object=function(fun=c(),.abcd="d",.useData=FALSE,...){
	return(functionList(fun=fun,.abcd=.abcd,.useData=.useData,...))
}