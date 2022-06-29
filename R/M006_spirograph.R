#' spirograph
#'
#' Creates a spirograph.
#' @param discSizes,discRotations spirograph parameters. 
#' @param lent,tmin,tmax passed to makeTSpace()
#' @param ... parameters passed to makePolar
#' @return Returns a component
#' @export
#' @examples
#' pruneStart(1:10,4)
makeSpirograph=function(discSizes=c(1,0.4),discRotations=c(3,25),tmin=0,tmax=2*pi,lent=1000,...){
	distanceFunction=list()
	bearingFunction=list()
	for(i in 1:length(discSizes)){
		fixedDistFun=paste("function(x){return(",discSizes[i],")}")
		distanceFunction[[i]]=eval(parse(text=fixedDistFun))
		fixedRotFun=paste("function(x){return(",discRotations[i],"*x)}")
		bearingFunction[[i]]=eval(parse(text=fixedRotFun))
	}
	return(makePolar(distanceFunction=distanceFunction,bearingFunction=bearingFunction,tmin=tmin,tmax=tmax,lent=lent,...))
}