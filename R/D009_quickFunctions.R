#' quickfunctions
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
#' @name quickfunctions
action.data=function(functions=c(),.abcd="a",.useData=TRUE,...){
	return(functionList(functions=c(),.abcd=.abcd,.useData=.useData,...)
}