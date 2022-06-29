#' makeShapeStringArt
#'
#' Creates string art whose points are on scaled variations of a starting shape and the lines are between the shapes.
#' @param x,y co-ordinates of the original shape. 
#' @param focus co-ordiantes of the centre to scale from. NULL, the default, uses c(mean(x),mean(y)) as the focus point.
#' @param scales scaling factors for the shapes.
#' @param distances,loop string art parameters
#' @return Returns a component that represents a piece of stringArt.
#' @family stringArt
#' @export
#' @examples
#' print(1+1)
makeShapeStringArt=function(x=c(0,1,1),y=c(0,0,1),focus=NULL,scales=(10:6)/10,distances=c(0,1,0,1),loop=rep(TRUE,4),...){
	names=c()
	answerx=c()
	answery=c()
	groups=list()
	lineGroups=list()
	if(is.null(focus)){
		focus=c(mean(x),mean(y))
	}
	for(i in 1:length(scales)){
		temp=(1:length(x))+length(names)
		names=c(names,temp)
		answerx=c(answerx,x*scales[i]+(1-scales[i])*focus[1])
		answery=c(answery,y*scales[i]+(1-scales[i])*focus[2])
		groups[[i]]=temp
		if(i<length(scales)){
			lineGroups[[i]]=c(i,i+1)
		}
	}
	names(groups)=1:length(scales)
	names=as.character(names)
	return(makeStringArt(x=answerx,y=answery,names=names,groups=groups,distances=distances,lineGroups=lineGroups,loop=loop,...))
}