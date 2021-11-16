#' calculateXYscale
#'
#' @description
#' A function to calculate the minimum/maximum co-ordinates of an artwork  object
#' @param art an artwork object
#' @param preserveAspectRatio logical
#' @param aspectRatio numerical
#' @param innerMargin numerical
#' @details 
#' This function is used by easyPlot to automatically calculate xlim and ylim if used on components or layers, or a list of such objects.
#' It runs all the functions in the artwork, so it will double the length of time it takes to plot an artwork. See easyPlot for more details.
#' @return returns a list containing two vectors x and y which contain minimum and maximum values. 
#' @export
#' @examples
#' print(1+1)
calculateXYscale=function(art,preserveAspectRatio=FALSE,aspectRatio=1,innerMargin=0.04){
	answer=list()
	temp=art

	oldSeed=saveSeed()
	if(class(art)=="artwork"){
		temp@mode=-1
		calculations=plotArtwork(temp)
	}else{
		calculations=easyPlot(temp,mode=-1)
	}
	restoreSeed(oldSeed)
	print(calculations)
	newXScale=calculations[[1]]
	newYScale=calculations[[2]]
	
	newXScale=c(newXScale[1]-(newXScale[2]-newXScale[1])*innerMargin,newXScale[2]+(newXScale[2]-newXScale[1])*innerMargin)
	newYScale=c(newYScale[1]-(newYScale[2]-newYScale[1])*innerMargin,newYScale[2]+(newYScale[2]-newYScale[1])*innerMargin)

	if(newXScale[1]==newXScale[2]){newXScale=newXScale+c(-1,1)}
	if(newYScale[1]==newYScale[2]){newYScale=newYScale+c(-1,1)}
	if(preserveAspectRatio){
		aspectRatioOfArt=(newYScale[2]-newYScale[1])/(newXScale[2]-newXScale[1])
		aspectRatioOfFile=aspectRatio
		if(class(art)=="artwork"){aspectRatioOfFile=art@height/art@width}
		if(aspectRatioOfArt<aspectRatioOfFile){
			answer$x=newXScale
			Ycenter=(newYScale[2]+newYScale[1])/2
			fixedY2=Ycenter+(newYScale[2]-Ycenter)*aspectRatioOfFile/aspectRatioOfArt
			fixedY1=Ycenter+(newYScale[1]-Ycenter)*aspectRatioOfFile/aspectRatioOfArt
			answer$y=c(fixedY1,fixedY2)
		}else{
			answer$y=newYScale
			Xcenter=(newXScale[2]+newXScale[1])/2
			fixedX2=Xcenter+(newXScale[2]-Xcenter)*aspectRatioOfArt/aspectRatioOfFile
			fixedX1=Xcenter+(newXScale[1]-Xcenter)*aspectRatioOfArt/aspectRatioOfFile
			answer$x=c(fixedX1,fixedX2)	
		}
	}else{
		answer$x=newXScale
		answer$y=newYScale
	}
	return(answer)
}