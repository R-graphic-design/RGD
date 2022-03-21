#' Chaos Game
#'
#' @description
#' runChaosGame calculates the points. makeChaosGame makes a component that calculates and plots the points.
#' @template param_type_points
#' @param ... parameters passed to the component and the component function runChaosGame. See "details" for more information.

#' @examples
#' art=makeChaosGame(decisionMode=2,cex=0.1,pch=21,runTime=10000)
#' #easyPlot(art,preserveAspectRatio=TRUE)
#' @name ChaosGame
NULL
#> NULL

#' @rdname ChaosGame
#' @export
makeChaosGame=function(type="points",...){
	return(component(type=type,...)+action.data("runChaosGame",...))
}

#' @rdname ChaosGame
#' @export
runChaosGame=function(inputx=0,inputy=0,x=NULL,y=NULL,attractor=list(x=c(-1,-1,1,1),y=c(-1,1,1,-1)),decisions=NULL,runTime=1000,sampleSize=NULL,reuseInput=FALSE,decisionMode=1){
	if(reuseInput|is.null(x)){x=inputx}
	if(reuseInput|is.null(y)){y=inputy}
	lenx=length(x)
	if(length(decisions)<runTime){
		optionsSet=seq_along(attractor$x)
		if(decisionMode==1){
			decisions=c(decisions,sample(optionsSet,size=runTime-length(decisions),replace=TRUE))
		}
		if(decisionMode==2){
			for(i in seq(from=length(decisions)+1,to=runTime)){
				if(i==1&&length(decisions)==0){
					decisions[1]=sample(optionsSet,size=1)
				}else{
					decisions[i]=sample(optionsSet[optionsSet!=decisions[i-1]],size=1)
				}
			}
		}
	}
	for(i in 1:runTime){
		x[lenx+i]=(x[lenx+i-1]+attractor$x[decisions[lenx+i]])/2
		y[lenx+i]=(y[lenx+i-1]+attractor$y[decisions[lenx+i]])/2
	}
	if(!is.null(sampleSize)){
		x=pruneStart(x,sampleSize)
		y=pruneStart(y,sampleSize)
	}
	return(list(x=x,y=y))
}