#' makeMandalaStringArt
#'
#' Creates string art whose points are on circles and the lines are between the circles.
#' @param x,y centre of the circles
#' @param radii the size of each circle. Length of this parameter controls the number of circles.
#' @param numPoints vector. The number of points on each circle
#' @param distances,loop string art parameters
#' @return Returns a component that represents a piece of stringArt.
#' @family stringArt
#' @export
#' @examples
#' print(1+1)
makeMandalaStringArt=function(x=0,y=0,radii=c(1,2,3,4,5),numPoints=c(16,16,16,16,16),distances=c(3,1,1,4),loop=rep(TRUE,4),...){
	names=c()
	answerx=c()
	answery=c()
	groups=list()
	lineGroups=list()
	for(i in 1:length(radii)){
		temp=(1:numPoints[i])+length(names)
		names=c(names,temp)
		xy=polar2cart(x,y,rep(radii[i],numPoints[i]),c(seq(0,2*pi,length.out=numPoints[i]+1))[1:numPoints[i]])
		answerx=c(answerx,xy$x)
		answery=c(answery,xy$y)
		groups[[i]]=temp
		if(i<length(radii)){
			lineGroups[[i]]=c(i,i+1)
		}
	}
	names(groups)=1:length(radii)
	names=as.character(names)
	return(makeStringArt(x=answerx,y=answery,names=names,groups=groups,distances=distances,lineGroups=lineGroups,loop=loop,...))
}