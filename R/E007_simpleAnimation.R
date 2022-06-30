#' simpleAnimation
#'
#' @description
#' Functions to create simple animations by varying parameters
#' @param input a component
#' @param parametersAdd a named list of parameters and changes. e.g. list(b=2,d=2) would add 2 to the value of b and d in input@data.
#' @param parametersMultiply a named list of parameters and scale factors. e.g. list(b=2,d=2) would double the value of b and d in input@data.
#' @param ... function parameters for parameterAdjust from addSimpleAnimation.
#' @details
#' @return parameterAdjust and addSimpleAnimation both return components
#' @family animation
#' @seealso
#' @examples
#' art=new("component",x=0,y=0)
#' art=addSimpleAnimation(art,list(x=0.01))
#' easyPlot(art,frames=1:100,framesToPlot=1:100,mode=0)
#' @name simpleAnimation
NULL
#> NULL


#' @rdname simpleAnimation
#' @export
parameterAdjust=function(input){
	if("parametersAdd"%in%names(input@data)){
		for(i in seq_along(input@data$parametersAdd)){
			input@data[[names(input@data$parametersAdd)[i]]]=input@data[[names(input@data$parametersAdd)[i]]]+input@data$parametersAdd[[i]]
		}
	}
	if("parametersMultiply"%in%names(input@data)){
		for(i in seq_along(input@data$parametersMultiply)){
			input@data[[names(input@data$parametersMultiply)[i]]]=input@data[[names(input@data$parametersMultiply)[i]]]*input@data$parametersMultiply[[i]]
		}
	}
	return(input)
}


#' @rdname simpleAnimation
#' @export
addSimpleAnimation=function(input,parametersAdd=NULL,parametersMultiply=NULL,...){
	if(!is.null(parametersAdd)){input@data$parametersAdd=parametersAdd}
	if(!is.null(parametersAdd)){input@data$parametersMultiply=parametersMultiply}
	input=input+action.object("parameterAdjust",...)
	return(input)
}


