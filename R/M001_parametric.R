#' makeParametric
#' @description
#' Functions to calculate the sequence of points given by functions x(t),y(t) for a vector of parameters t.
#' runParametric calculates the points. makeParametric makes a component that calculates and plots the output. runTSpace creates a set of parameters if one isn't provided.
#' @param xFun,yFun functions or names of functions.
#' @param parameterSpace the set of parameters used.
#' @param tmin,tmax,lent used when parameterSpace isn't given. seq(tmin,tmax,lent) is then used as a parameterSpace.
#' @param ... parameters passed to the component and the component functions runParametric and runTSpace. See "details" for more information.
#' @examples
#' easyPlot(makeParametric())
#' @family makeParametric
#' @name makeParametric 
NULL
#> NULL

#' @rdname makeParametric
#' @export
runParametric=function(xFun=function(x){sin(x*2*pi)},yFun=function(x){sin(x*4*pi)},parameterSpace){
	answer=list(x=do.call(xFun,list(parameterSpace)),y=do.call(yFun,list(parameterSpace)))
	return(answer)
}
#' @rdname makeParametric
#' @export
makeTSpace=function(tmin=0,tmax=1,lent=101){
	return(list(parameterSpace=seq(tmin,tmax,length.out=lent)))
}
#' @rdname makeParametric
#' @export
makeParametric=function(type="lines",...){
	if("parameterSpace"%in%names(list(...))){
		return(component(type=type,...)+action.data(fun="runParametric",...))
	}else{
		return(component(type=type,...)+action.data(fun=list("makeTSpace","runParametric"),...))
	}
}