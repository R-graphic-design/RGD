#' makeGallery
#'
#' Takes a list of components and/or layers and creates a section that contains a grid of sections which each contain one item from the original list.
#' @param inputList
#' @param nRow
#' @param width,height,x,y parameters for the final section
#' @param xscale,yscale a scale to use for each child section. If not provided these are calculated. See useGlobalScale.
#' @param useGlobalScale if FALSE child scaling calculations are independent. If TRUE, scales are adjusted so each child has the same scales, and all components are visible. Ignored if xscale and yscale are provided.
#' @param units provided to each child section
#' @param preserveAspectRatio logical. Used if scales are automatically calculated.
#' @return Returns a section object.
#' @export
#' @examples
#' pruneStart(1:10,4)
makeGallery=function(inputList,nRow,
					width=unit(1,"npc"),height=unit(1,"npc"),
					x=unit(0,"npc"),y=unit(0,"npc"),
					xscale=NULL,yscale=NULL,useGlobalScale=FALSE,
					units=list(),preserveAspectRatio=FALSE){
	nCol=ceiling(length(inputList)/nRow)
	sectionList=list()
	sectionWidth=unit(as.numeric(width)/nCol,grid::unitType(width))
	sectionHeight=unit(as.numeric(height)/nRow,grid::unitType(height))

	for(i in 1:length(inputList)){
		if(class(inputList[[i]])=="component"){
			inputList[[i]]=new("layer",components=inputList[i])
		}
		sectionList[[i]]=new("section",
							width=sectionWidth,height=sectionHeight,
							x=unit(as.numeric(x)+((i-1)%%nCol)*as.numeric(width)/nCol,grid::unitType(x)),
							y=unit(as.numeric(y)+as.numeric(height)-(floor((i-1)/nCol)+1)*as.numeric(height)/nRow,grid::unitType(y)),
							tags="gallerySection",just=0,units=units)
		if(class(inputList[[i]])=="layer"){
			sectionList[[i]]@layers=inputList[i]
			if(is.null(xscale)){
				temp=calculateXYscale(inputList[[i]],preserveAspectRatio)
				sectionList[[i]]@xscale=temp$x
				sectionList[[i]]@yscale=temp$y
			}else{
				sectionList[[i]]@xscale=xscale
				sectionList[[i]]@yscale=yscale
			}
		}else{
			sectionList[[i]]@sections=inputList[i]
		}
	}
	if(useGlobalScale){
		xMin=sectionList[[1]]@xscale[1]
		xMax=sectionList[[1]]@xscale[2]
		yMin=sectionList[[1]]@yscale[1]
		yMax=sectionList[[1]]@yscale[2]
		for(i in 2:length(sectionList)){
			#xmin
			if(sectionList[[i]]@xscale[1]<xMin){
				xMin=sectionList[[i]]@xscale[1]
				for(j in 1:(i-1)){
					sectionList[[j]]@xscale[1]=xMin
				}
			}else{
				sectionList[[i]]@xscale[1]=xMin
			}
			#xmax
			if(sectionList[[i]]@xscale[2]>xMax){
				xMax=sectionList[[i]]@xscale[2]
				for(j in 1:(i-1)){
					sectionList[[j]]@xscale[2]=xMax
				}
			}else{
				sectionList[[i]]@xscale[2]=xMax
			}
			#yMin
			if(sectionList[[i]]@yscale[1]<yMin){
				yMin=sectionList[[i]]@yscale[1]
				for(j in 1:(i-1)){
					sectionList[[j]]@yscale[1]=yMin
				}
			}else{
				sectionList[[i]]@yscale[1]=yMin
			}
			#yMax
			if(sectionList[[i]]@yscale[2]>yMax){
				yMax=sectionList[[i]]@yscale[2]
				for(j in 1:(i-1)){
					sectionList[[j]]@yscale[2]=yMax
				}
			}else{
				sectionList[[i]]@yscale[2]=yMax
			}
		}
	}
	answer=new("section",sections=sectionList,width=width,height=height,x=x,y=y,just=0)
	return(answer)
}
