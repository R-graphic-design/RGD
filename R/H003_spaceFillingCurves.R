#' Space Filling Curves
#'
#' @description
#' Functions to calculate the sequence of points given by the following dynamical system in order to draw the attractor of the system.
#' @param 
#' @param
#' @param
#' @param
#' @param ... parameters passed to the component and the component function runSpaceFillingCurve. See "details" for more information.
#' @details
#' @return make returns a component, run returns a list.
#' @family fractals 
#' @seealso
#' @examples
#' print(1+1)
#' @name spaceFillingCurve
NULL

#' @rdname spaceFillingCurve
#' @export
makeSpaceFillingCurve=function(type="segments",...){
	answer=component(type=type,...)+action.data("runSpaceFillingCurve",...)
	return(answer)
}

#' @rdname spaceFillingCurve
#' @export
runSpaceFillingCurve=function(x0,y0,x1,y1,
			percentDistances,bearing,addFinalSegment=TRUE,
			lineFlip=lineFlip,segmentsToFlip=c(),segmentOrder=c()){
	numChildren=ifelse(addFinalSegment,length(percentDistances)+1,length(percentDistances))
	blankAnswer=rep(0,times=numChildren*length(x0))
	answerX0=blankAnswer
	answerY0=blankAnswer
	answerX1=blankAnswer
	answerY1=blankAnswer
	answerX0[(seq_along(x0)-1)*numChildren+1]=x0
	answerY0[(seq_along(y0)-1)*numChildren+1]=y0
	answerX1[(seq_along(x0))*numChildren]=x1
	answerY1[(seq_along(y0))*numChildren]=y1
	temp=cart2polar(x0=x0,y0=y0,x1=x1,y1=y1)
	dists=percentDistances%o%temp$r

	angles=outer(bearing,temp$bearing,"+")
	
	#UPDATE THIS
	segmentsToFlipAnswer=c()
	if(length(segmentsToFlip)>0){
		print("FLIPPING")
		for(i in seq_along(segmentsToFlip))
		segmentsToFlipAnswer=c(segmentsToFlipAnswer,(segmentsToFlip[i]-1)*numChildren+1:numChildren)
	}
	
	if(length(segmentOrder)==0){segmentOrder=seq_along(x0)}
	segmentOrder=as.list(segmentOrder)
	
	
	print("SEGMENTS TO FLIP TEMP")
	print(segmentsToFlipAnswer)
	print(segmentsToFlip)
	#for dists and angles each column represents an original segment, rows = parts of the split
	for(i in seq_along(answerX0)){
		segmentNumber=floor((i-1)/numChildren)+1
		childNumber=loopIndex(i,numChildren)

		autoFinalSegment=addFinalSegment&&(childNumber>nrow(dists))
		
		if(childNumber==1){
			answerX0[i]=x0[segmentNumber]
			answerY0[i]=y0[segmentNumber]
		}else{
			answerX0[i]=answerX1[i-1]
			answerY0[i]=answerY1[i-1]
		}
		
		if(autoFinalSegment){
			answerX1[i]=x1[segmentNumber]
			answerY1[i]=y1[segmentNumber]
		}else{
			temp=polar2cart(answerX0[i],answerY0[i],dists[childNumber,segmentNumber],angles[childNumber,segmentNumber])
			answerX1[i]=temp$x
			answerY1[i]=temp$y
		}
	}
	for(i in seq_along(x0)){
		originalOrder=segmentOrder[[i]]
		segmentOrder[[i]]=(segmentOrder[[i]]-1)*numChildren+1:numChildren
		if(originalOrder %in% segmentsToFlip){
			segmentOrder[[i]]=rev(segmentOrder[[i]])
			#answerX0[(i-1)*numChildren+1:numChildren]=rev(answerX0[(i-1)*numChildren+1:numChildren])
			#answerY0[(i-1)*numChildren+1:numChildren]=rev(answerY0[(i-1)*numChildren+1:numChildren])
			#answerX1[(i-1)*numChildren+1:numChildren]=rev(answerX1[(i-1)*numChildren+1:numChildren])
			#answerY1[(i-1)*numChildren+1:numChildren]=rev(answerY1[(i-1)*numChildren+1:numChildren])
		}
	}
	for(i in seq_along(answerX0)){
		if(loopSubset(lineFlip,i)){
			tempX0=answerX0[i]
			tempY0=answerY0[i]
			answerX0[i]=answerX1[i]
			answerY0[i]=answerY1[i]
			answerX1[i]=tempX0
			answerY1[i]=tempY0
			if(i %in% segmentsToFlipAnswer){
				segmentsToFlipAnswer=segmentsToFlipAnswer[-which(segmentsToFlipAnswer==i)]
			}else{
				segmentsToFlipAnswer=c(segmentsToFlipAnswer,i)
			}
		}
	}
	print("SEGMENTS TO FLIP ANSWER")
	print(segmentsToFlipAnswer)
	print(segmentsToFlip)
	
	print("SEGMENT ORDER")
	print(unlist(segmentOrder))
	
	return(list(x0=answerX0,y0=answerY0,x1=answerX1,y1=answerY1,segmentsToFlip=segmentsToFlipAnswer,segmentOrder=unlist(segmentOrder)))
}

