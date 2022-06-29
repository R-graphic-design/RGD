#' maurerRose
#'
#' Creates a Maurer rose.
#' @param petalNumber,degreesParameter maurer rose parameters. 
#' @param lent,tmin,tmax passed to makeTSpace()
#' @param ... parameters passed to makePolar
#' @return Returns a component
#' @export
#' @examples
#' pruneStart(1:10,4)
makeMaurerRose=function(petalNumber=2,degreesParameter=39,lent=361,tmin=0,tmax=2*pi,...){
	fixedDistFun=paste("function(x){return(sin(",degreesParameter,"*",petalNumber,"*x))}")
	distanceFunction=eval(parse(text=fixedDistFun))
	fixedRotFun=paste("function(x){return(",degreesParameter,"*x)}")
	bearingFunction=eval(parse(text=fixedRotFun))
	return(makePolar(distanceFunction=distanceFunction,bearingFunction=bearingFunction,tmin=tmin,tmax=tmax,lent=lent,...))
}