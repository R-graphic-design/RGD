#' makeHarmonograph
#' @description
#' Functions to calculate a harmonograph
#' runHarmonograph calculates the points. makeHarmonograph makes a component that calculates and plots the output.
#' @param frequencies
#' @param phases
#' @param amplitudes
#' @param decay
#' @param ... parameters passed to makeParametric.
#' @examples
#' @export
#' @family makeParametric
#' easyPlot(makeHarmonograph())
#' @name makeHarmonograph
makeHarmonograph=function(frequencies=c(3,0,4,0),phases=c(pi/2,0,0,0),amplitudes=c(1,0,1,0),decay=c(0,0,0,0),...){
	xFun=paste("function(x){return(",amplitudes[1],"*sin(",frequencies[1],"*x+",phases[1],")*exp(-",decay[1],"*x)+",amplitudes[2],"*sin(",frequencies[2],"*x+",phases[2],")*exp(-",decay[2],"*x)     )}")
	yFun=paste("function(x){return(",amplitudes[3],"*sin(",frequencies[3],"*x+",phases[3],")*exp(-",decay[3],"*x)+",amplitudes[4],"*sin(",frequencies[4],"*x+",phases[4],")*exp(-",decay[4],"*x)     )}")

	return(makeParametric(xFun=eval(parse(text=xFun)),yFun=eval(parse(text=yFun)),...))
}