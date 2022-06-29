#' Celtic Knots
#'
#' @description
#' Functions to draw Celtic knots.
#' @param layer
#' @param ... parameters passed to the layer and the layer function object. See "details" for more information.
#' @param lineOrder
#' @param angles
#' @param basis
#' @param angles
#' @param knotOrder
#' @param knotFillColour
#' @param knotBorderColour
#' @param clockWiseCorners
#' @param flipCorners
#' @param distance
#' @param knotMode
#' @details
#' @return a celtic knot
#' @seealso
#' @examples
#' print(1+1)
#' @name celticKnots
NULL

#' @rdname celticKnots
#' @export
startCelticRibbon=function(layer){
	print(1)
	distance=layer@data$distance
	if(!is.matrix(distance)){distance=matrix(distance)}
	midLine=list()
	basis=layer@data$basis
	lineOrder=layer@data$lineOrder
	for(i in seq_along(lineOrder)){
		if(i>1&&lineOrder[i]%in%lineOrder[1:(i-1)]){
			oldName=lineOrder[i]
			lineOrder[i]=paste(lineOrder[i],i,sep="_")
			basis[nrow(basis)+1,1]=lineOrder[i]
			basis[nrow(basis),2:3]=basis[basis$name==oldName,2:3]
		}
	}

	loopKnot=TRUE
	if(loopKnot){
		lineOrder=c(lineOrder,lineOrder[1])
	}

	
	print("TESTING--DUPLICATE POINTS")
	print(basis)
	print(lineOrder)
	
	angles=layer@data$angles
	fromPoints=lineOrder[1:(length(lineOrder)-1)]
	toPoints=lineOrder[2:length(lineOrder)]
	if("cuts"%in%names(layer@data)){
		toPoints[c(layer@data$cuts,length(toPoints))]=toPoints[c(length(toPoints),layer@data$cuts)]
	}
	ribbons=list()
	knotMode="DEFAULT"
	if("knotMode"%in%names(layer@data)){knotMode=layer@data$knotMode}
	print(2)
	for(i in 1:length(fromPoints)){
		midLine[[i]]=new("component",type="lines",x=c(basis[basis$name==fromPoints[i],"x"],basis[basis$name==toPoints[i],"x"]),y=c(basis[basis$name==fromPoints[i],"y"],basis[basis$name==toPoints[i],"y"]))
	}
	print(3)
	print(midLine[[1]]@data$x)
	
	#FORCE ANY EXIST ANGLES TO THE RANGE [0,2pi) and put them in the component.
	if(!is.null(angles)){
		for(i in 1:nrow(angles)){
			print(paste("using ANGLES",i))
			midLine[[ angles[i,"angleNumber"] ]]@data$inAngle1=angles[i,"inAngle1"]#%%(2*pi)#force to [0,2pi)
			midLine[[ angles[i,"angleNumber"] ]]@data$inAngle2=angles[i,"inAngle2"]#%%(2*pi)#force to [0,2pi)
			midLine[[ angles[i,"angleNumber"] ]]@data$angles=c(angles[i,"inAngle1"],angles[i,"inAngle2"])
			#midLine[[ angles[i,"angleNumber"] ]]@data$angles=c(angles[i,"inAngle1"]%%(2*pi),angles[i,"inAngle2"]%%(2*pi))

		}
	}
	innerRibbonRatio=layer@data$innerRibbonRatio
	if(is.null(innerRibbonRatio)){innerRibbonRatio=0.5}
	print(4)
	print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)

	for(i in 1:length(midLine)){
		if(!"inAngle1"%in%names(midLine[[i]]@data)){
			print(paste("use natural angle",i))
			naturalAngle=0
			if((midLine[[i]]@data$x[2]-midLine[[i]]@data$x[1])==0){
				if(midLine[[i]]@data$y[2]>midLine[[i]]@data$y[1]){
					naturalAngle=pi/2
				}else{
					naturalAngle=3*pi/2
				}
			}else{
				naturalAngle=atan((midLine[[i]]@data$y[2]-midLine[[i]]@data$y[1])/(midLine[[i]]@data$x[2]-midLine[[i]]@data$x[1]))
				if((midLine[[i]]@data$x[2]-midLine[[i]]@data$x[1])<0){
					naturalAngle=naturalAngle+pi
				}else{
					if((midLine[[i]]@data$y[2]-midLine[[i]]@data$y[1])<0){
						naturalAngle=naturalAngle+2*pi
					}
				}
			}
			midLine[[ i ]]@data$inAngle1=naturalAngle
			midLine[[ i ]]@data$inAngle2=naturalAngle
			midLine[[i]]@data$angles=c(naturalAngle,naturalAngle)
		}
	}#THE CODE BELOW IS OLD??? WAS INSIDE THIS }
#	if(midLine[[i]]@data$x[1]<midLine[[i]]@data$x[2]){
#			print(paste("add pi to inAngle2",i))
#			midLine[[i]]@data$lineDirection=1
#	}else if(midLine[[i]]@data$x[1]>midLine[[i]]@data$x[2]){
#			midLine[[i]]@data$lineDirection=-1
#	}else{
#			midLine[[i]]@data$lineDirection=0
	#}
	#if(midLine[[i]]@data$x[1]>midLine[[i]]@data$x[2]){
	#			print(paste("add pi to inAngle1",i))
	#		midLine[[i]]@data$inAngle1=midLine[[i]]@data$inAngle1+pi
	#}

	print(5)
	print(length(midLine))
	print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)

	for(i in 1:length(midLine)){
		#New code, handles cuts
		fromPoint=fromPoints[i]
		toPoint=toPoints[i]
		beforeSegment=which(toPoints==fromPoint)
		afterSegment=which(fromPoints==toPoint)
		midLine[[ i ]]@data$beforeAngle=midLine[[ beforeSegment ]]@data$inAngle2
		midLine[[ i ]]@data$afterAngle=midLine[[ afterSegment ]]@data$inAngle1	
		if(i==1){print("beforeAFterAngle");print(midLine[[1]]@data$beforeAngle);print(midLine[[1]]@data$afterAngle)}
		
		
	
	
	
#	if(midLine[[i]]@data$lineDirection==1 && midLine[[loopIndex(i-1,length(midLine))]]@data$lineDirection==1){
#		midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1+pi/2
#		#midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi/2
#		}
#	if(midLine[[i]]@data$lineDirection==1 && midLine[[loopIndex(i+1,length(midLine))]]@data$lineDirection==1){
#		midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi/2
#		#midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi/2
	

#	}
	#if(midLine[[i]]@data$inAngle2<midLine[[i]]@data$afterAngle){midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi/2}
	#if(midLine[[i]]@data$inAngle1>midLine[[i]]@data$beforeAngle){midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1+pi}
	#if(midLine[[i]]@data$afterAngle>midLine[[i]]@data$inAngle2){midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi}
	
	#CURRENTLY USING CLOCKWISE CORNERS, CAN DEFUNCT BY REPLACING ABOVE AVERAGE WITH IF STATEMENTS??
	#NOT SURE WHY SOME CORNERS NEED FLIPPING???????
	
	#BIG TEST REMOVE ALL CLOCKWISE CORNERS, now all get pi/2, and no flip corners...
	
		#NEW IDEA FLIP IF ADDING pi/2 moves it to other side of the angle..
		turningAngle1=(midLine[[i]]@data$beforeAngle-midLine[[i]]@data$inAngle1)/2
		turningAngle2=(midLine[[i]]@data$inAngle2-midLine[[i]]@data$afterAngle)/2
		
		
		
		#sidePoint angles give a direction for ribbons to start at
		midLine[[i]]@data$sidePointAngle1=((midLine[[i]]@data$inAngle1+midLine[[i]]@data$beforeAngle+pi)/2)
		midLine[[i]]@data$sidePointAngle2=((midLine[[i]]@data$inAngle2+midLine[[i]]@data$afterAngle+pi)/2)
		
		if((midLine[[i]]@data$sidePointAngle1-midLine[[i]]@data$beforeAngle)%%(2*pi)>pi){
			midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1-pi
		}
		if((midLine[[i]]@data$sidePointAngle2-midLine[[i]]@data$inAngle2)%%(2*pi)>pi){
			midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2-pi
		}
		#if((turningAngle1%%pi)==floor((turningAngle1+pi/2)/pi)){
		#	midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1+pi/2
		#}else{
		#	midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1-pi/2		
		#}
		#if(floor(turningAngle2/pi)==floor((turningAngle2+pi/2)/pi)){
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi/2
		#}else{
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2-pi/2	
		#}
		#if(i %in% layer@data$clockWiseCorners){
		#	#anti-clockwise flip so angle 1 is always on the left??
			
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi		
		#}
		#if(loopIndex(i+1,length(midLine)) %in% layer@data$clockWiseCorners){
		#	#anti-clockwise flip so angle 1 is always on the left??
			
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi		
		#}
		#if(i %in% layer@data$flipCorners){
		##anti-clockwise flip so angle 1 is always on the left??
		#	midLine[[i]]@data$sidePointAngle1=midLine[[i]]@data$sidePointAngle1+pi
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi		
		#}
		#if(loopIndex(i+1,length(midLine)) %in% layer@data$flipCorners){
		#	#anti-clockwise flip so angle 1 is always on the left??
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi
		#	midLine[[i]]@data$sidePointAngle2=midLine[[i]]@data$sidePointAngle2+pi		
		#}
	
		if(i==1){print(midLine[[1]]@data$sidePointAngle1);print("1 SidePointAngle 2");print(midLine[[1]]@data$sidePointAngle2);print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)}	
		#addX1=ifelse(abs(midLine[[i]]@data$sidePointAngle1%%pi-pi/2)>pi/4,sign(cos(midLine[[i]]@data$sidePointAngle1)),cos(midLine[[i]]@data$sidePointAngle1)/cos(pi/4))*distance
		#addY1=ifelse(abs(midLine[[i]]@data$sidePointAngle1%%pi-pi/2)<pi/4,sign(sin(midLine[[i]]@data$sidePointAngle1)),sin(midLine[[i]]@data$sidePointAngle1)/sin(pi/4))*distance
		#addX2=ifelse(abs(midLine[[i]]@data$sidePointAngle2%%pi-pi/2)>pi/4,sign(cos(midLine[[i]]@data$sidePointAngle2)),cos(midLine[[i]]@data$sidePointAngle2)/cos(pi/4))*distance
		#addY2=ifelse(abs(midLine[[i]]@data$sidePointAngle2%%pi-pi/2)<pi/4,sign(sin(midLine[[i]]@data$sidePointAngle2)),sin(midLine[[i]]@data$sidePointAngle2)/sin(pi/4))*distance

		#calculates x,y changes in start points for the default ribbon, using the distance matrix. (can have different distances at different points, if desired.)
		#the last part is strange because it can be matrix(0.2) or matrix(1:10,c(0.2,.....),dim=c(10,2))
		#distanceFactor corrects the thickness of the ribbon
		angleDifference1=abs(midLine[[i]]@data$inAngle1-midLine[[i]]@data$beforeAngle)
		angleDifference2=abs(midLine[[i]]@data$inAngle2-midLine[[i]]@data$afterAngle)
		distanceFactor1=abs(sin(angleDifference1/2)*tan(angleDifference1/2)+cos(angleDifference1/2))
		distanceFactor2=abs(sin(angleDifference2/2)*tan(angleDifference2/2)+cos(angleDifference2/2))
		
		
		addX1=cos(midLine[[i]]@data$sidePointAngle1)*distance[loopIndex(i,nrow(distance)),1]*distanceFactor1
		addY1=sin(midLine[[i]]@data$sidePointAngle1)*distance[loopIndex(i,nrow(distance)),loopIndex(2,ncol(distance))]*distanceFactor1
		addX2=cos(midLine[[i]]@data$sidePointAngle2)*distance[loopIndex(i-1,nrow(distance)),1]*distanceFactor2
		addY2=sin(midLine[[i]]@data$sidePointAngle2)*distance[loopIndex(i-1,nrow(distance)),loopIndex(2,ncol(distance))]*distanceFactor2
	
		if(i==1){print(midLine[[1]]@data$x);print(addX1);print(addY2);print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)
	#When knot changes direction from left to right, need to flip things.
	#beforeAngleFacesLeft=abs(abs(midLine[[i]]@data$beforeAngle)%%(2*pi))>pi
	#afterAngleFacesLeft=abs(abs(midLine[[i]]@data$afterAngle)%%(2*pi))>pi
	#if(beforeAngleFacesLeft+afterAngleFacesLeft==1){
	#	print("CORNER FLIP")
	#	temp=addX1
	#	addX1=addX2
	#	addX2=temp
	#}
	}#end i making midlines

#if("flipCorners"%in%names(layer@data)&&i%in%layer@data$flipCorners){
#print("CORNER FLIP")
#temp=addX1
#addX1=addX2
#addX2=temp
#}

	print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)
	
	#MAKE STARTING POINTS FOR ALL THE RIBBONS
		if(knotMode=="DEFAULT"){
			ribbons[[2*i-1]]=midLine[[i]]
			ribbons[[2*i]]=midLine[[i]]
			ribbons[[2*i-1]]@data$x=ribbons[[2*i-1]]@data$x+c(addX1,addX2)
			ribbons[[2*i-1]]@data$y=ribbons[[2*i-1]]@data$y+c(addY1,addY2)
			ribbons[[2*i]]@data$x=ribbons[[2*i]]@data$x-c(addX1,addX2)
			ribbons[[2*i]]@data$y=ribbons[[2*i]]@data$y-c(addY1,addY2)
		}
		if(knotMode=="INNERRIBBON"){
			ribbons[[4*i-3]]=midLine[[i]]
			ribbons[[4*i-2]]=midLine[[i]]
			ribbons[[4*i-1]]=midLine[[i]]
			ribbons[[4*i]]=midLine[[i]]
			ribbons[[4*i-3]]@data$x=ribbons[[4*i-3]]@data$x+c(addX1,addX2)
			ribbons[[4*i-3]]@data$y=ribbons[[4*i-3]]@data$y+c(addY1,addY2)
			ribbons[[4*i-2]]@data$x=ribbons[[4*i-2]]@data$x+c(addX1,addX2)*innerRibbonRatio
			ribbons[[4*i-2]]@data$y=ribbons[[4*i-2]]@data$y+c(addY1,addY2)*innerRibbonRatio
			ribbons[[4*i-1]]@data$x=ribbons[[4*i-1]]@data$x-c(addX1,addX2)*innerRibbonRatio
			ribbons[[4*i-1]]@data$y=ribbons[[4*i-1]]@data$y-c(addY1,addY2)*innerRibbonRatio
			ribbons[[4*i]]@data$x=ribbons[[4*i]]@data$x-c(addX1,addX2)
			ribbons[[4*i]]@data$y=ribbons[[4*i]]@data$y-c(addY1,addY2)
		}
		if(knotMode=="DOUBLE"){
			ribbons[[3*i-2]]=midLine[[i]]
			ribbons[[3*i-1]]=midLine[[i]]
			ribbons[[3*i]]=midLine[[i]]
			ribbons[[3*i-2]]@data$x=ribbons[[3*i-2]]@data$x+c(addX1,addX2)
			ribbons[[3*i-2]]@data$y=ribbons[[3*i-2]]@data$y+c(addY1,addY2)
			ribbons[[3*i-1]]@data$x=ribbons[[3*i-1]]@data$x-c(addX1,addX2)
			ribbons[[3*i-1]]@data$y=ribbons[[3*i-1]]@data$y-c(addY1,addY2)
		}
	}
	print(6)
	layer@components=ribbons
	print(7)
	print(midLine[[1]]@data$inAngle1);print(midLine[[1]]@data$inAngle2)	
	print("DDD")
	print(layer@components[[1]]@data)
	print("DDD")
	return(layer)
}





#' @rdname celticKnots
#' @export
celticBezierCurve=function(layer){
	bezierPoints=list()
	for(i in 1:length(layer@components)){
		if("angles"%in%names(layer@components[[i]]@data)){
			print(paste("bezierKNOT i=",i))
			print(paste("angles",layer@components[[i]]@data$angles))
			bezierPoints[[i]]=bezierInterpolate4Points(layer@components[[i]]@data$x,layer@components[[i]]@data$y,layer@components[[i]]@data$angles,tseq=c(seq(0,1,length.out=50)))
			layer@components[[i]]@data$x=bezierPoints[[i]][,1]
			layer@components[[i]]@data$y=bezierPoints[[i]][,2]
		}
	}
	return(layer)
}


#' @rdname celticKnots
#' @export
loopToRibbon=function(layer){
	answer=layer
	if(layer@data$knotMode=="DEFAULT"){
		for(i in 1:(length(layer@components)/2)){
			answer@components[[3*i-2]]=layer@components[[2*i-1]]
			answer@components[[3*i-1]]=layer@components[[2*i]]
			answer@components[[3*i-2]]@type="celticKnotBorder"
			answer@components[[3*i-1]]@type="celticKnotBorder"
			#answer@components[[3*i-1]]@data$x=answer@components[[3*i-1]]@data$x[c(-1,-length(answer@components[[3*i-1]]@data$x)+0)]
			#answer@components[[3*i-1]]@data$y=answer@components[[3*i-1]]@data$y[c(-1,-length(answer@components[[3*i-1]]@data$y)+0)]
			#answer@components[[3*i-2]]@data$x=answer@components[[3*i-2]]@data$x[c(-1,-length(answer@components[[3*i-2]]@data$x)+0)]
			#answer@components[[3*i-2]]@data$y=answer@components[[3*i-2]]@data$y[c(-1,-length(answer@components[[3*i-2]]@data$y)+0)]

			
			sectionColours=layer@data$knotFillColour[[loopIndex(i,length(layer@data$knotFillColour))]]
			answer@components[[3*i]]=new("component",type="celticKnotFill",border=NA,x=c(),y=c(),col=sectionColours)
			for(j in 1:length(sectionColours)){
				index2=ceiling((j/length(sectionColours))*length(layer@components[[2*i]]@data$x))
				index1=ceiling(((j-1)/length(sectionColours))*length(layer@components[[2*i]]@data$x))
				if(j==1){index1=1}
				newXside1=layer@components[[2*i-1]]@data$x[index1:index2]
				newXside2=layer@components[[2*i]]@data$x[index2:index1]
				newYside1=layer@components[[2*i-1]]@data$y[index1:index2]
				newYside2=layer@components[[2*i]]@data$y[index2:index1]
				answer@components[[3*i]]@data$x=c(answer@components[[3*i]]@data$x,newXside1,newXside2)
				answer@components[[3*i]]@data$y=c(answer@components[[3*i]]@data$y,newYside1,newYside2)
				if(j<length(sectionColours)){
						answer@components[[3*i]]@data$x=c(answer@components[[3*i]]@data$x,NA)
						answer@components[[3*i]]@data$y=c(answer@components[[3*i]]@data$y,NA)
				}
			}
		}
		answer@data$numPiecesPerSection=3
		return(answer)
	}
	if(layer@data$knotMode=="INNERRIBBON"){
		for(i in 1:(length(layer@components)/4)){
			answer@components[[6*i-5]]=layer@components[[4*i-3]]
			answer@components[[6*i-4]]=layer@components[[4*i]]
			answer@components[[6*i-5]]@type="celticKnotBorder"
			answer@components[[6*i-4]]@type="celticKnotBorder"
			answer@components[[6*i-2]]=layer@components[[4*i-2]]
			answer@components[[6*i-1]]=layer@components[[4*i-1]]
			answer@components[[6*i-2]]@type="celticKnotBorder"
			answer@components[[6*i-1]]@type="celticKnotBorder"
			#answer@components[[3*i-1]]@data$x=answer@components[[3*i-1]]@data$x[c(-1,-length(answer@components[[3*i-1]]@data$x)+0)]
			#answer@components[[3*i-1]]@data$y=answer@components[[3*i-1]]@data$y[c(-1,-length(answer@components[[3*i-1]]@data$y)+0)]
			#answer@components[[3*i-2]]@data$x=answer@components[[3*i-2]]@data$x[c(-1,-length(answer@components[[3*i-2]]@data$x)+0)]
			#answer@components[[3*i-2]]@data$y=answer@components[[3*i-2]]@data$y[c(-1,-length(answer@components[[3*i-2]]@data$y)+0)]

			
			sectionColours=layer@data$knotFillColour[[loopIndex(i,length(layer@data$knotFillColour))]]
			answer@components[[6*i-3]]=new("component",type="celticKnotFill",border=NA,x=c(),y=c(),col=sectionColours)
			for(j in 1:length(sectionColours)){
				index2=ceiling((j/length(sectionColours))*length(layer@components[[4*i]]@data$x))
				index1=ceiling(((j-1)/length(sectionColours))*length(layer@components[[4*i]]@data$x))
				if(j==1){index1=1}
				newXside1=layer@components[[4*i-3]]@data$x[index1:index2]
				newXside2=layer@components[[4*i]]@data$x[index2:index1]
				newYside1=layer@components[[4*i-3]]@data$y[index1:index2]
				newYside2=layer@components[[4*i]]@data$y[index2:index1]
				answer@components[[6*i-3]]@data$x=c(answer@components[[6*i-3]]@data$x,newXside1,newXside2)
				answer@components[[6*i-3]]@data$y=c(answer@components[[6*i-3]]@data$y,newYside1,newYside2)
				if(j<length(sectionColours)){
						answer@components[[6*i-3]]@data$x=c(answer@components[[6*i-3]]@data$x,NA)
						answer@components[[6*i-3]]@data$y=c(answer@components[[6*i-3]]@data$y,NA)
				}
			}
			innerRibbonColours="black"
			if("innerRibbonColours"%in%names(layer@data)){innerRibbonColours=layer@data$innerRibbonColours[[loopIndex(i,length(layer@data$innerRibbonColours))]]}
			answer@components[[6*i]]=new("component",type="celticKnotFill",border=NA,x=c(),y=c(),col=innerRibbonColours)
			for(j in 1:length(sectionColours)){
				index2=ceiling((j/length(sectionColours))*length(layer@components[[4*i]]@data$x))
				index1=ceiling(((j-1)/length(sectionColours))*length(layer@components[[4*i]]@data$x))
				if(j==1){index1=1}
				newXside1=layer@components[[4*i-2]]@data$x[index1:index2]
				newXside2=layer@components[[4*i-1]]@data$x[index2:index1]
				newYside1=layer@components[[4*i-2]]@data$y[index1:index2]
				newYside2=layer@components[[4*i-1]]@data$y[index2:index1]
				answer@components[[6*i]]@data$x=c(answer@components[[6*i]]@data$x,newXside1,newXside2)
				answer@components[[6*i]]@data$y=c(answer@components[[6*i]]@data$y,newYside1,newYside2)
				if(j<length(sectionColours)){
						answer@components[[6*i]]@data$x=c(answer@components[[6*i]]@data$x,NA)
						answer@components[[6*i]]@data$y=c(answer@components[[6*i]]@data$y,NA)
				}
			}
		}
		answer@data$numPiecesPerSection=6
		return(answer)
	}
	if(layer@data$knotMode=="DOUBLE"){
		#3*i = og midline, -1/-2 = alternate sides
		for(i in 1:(length(layer@components)/3)){
			answer@components[[5*i-4]]=layer@components[[3*i-2]]
			answer@components[[5*i-3]]=layer@components[[3*i-1]]
			answer@components[[5*i-2]]=layer@components[[3*i]]
			answer@components[[5*i-4]]@type="celticKnotBorder"
			answer@components[[5*i-3]]@type="celticKnotBorder"
			answer@components[[5*i-2]]@type="celticKnotBorder"
			#answer@components[[3*i-1]]@data$x=answer@components[[3*i-1]]@data$x[c(-1,-length(answer@components[[3*i-1]]@data$x)+0)]
			#answer@components[[3*i-1]]@data$y=answer@components[[3*i-1]]@data$y[c(-1,-length(answer@components[[3*i-1]]@data$y)+0)]
			#answer@components[[3*i-2]]@data$x=answer@components[[3*i-2]]@data$x[c(-1,-length(answer@components[[3*i-2]]@data$x)+0)]
			#answer@components[[3*i-2]]@data$y=answer@components[[3*i-2]]@data$y[c(-1,-length(answer@components[[3*i-2]]@data$y)+0)]

			
			sectionColours=layer@data$knotFillColour[[loopIndex(i,length(layer@data$knotFillColour))]]
			sectionColours2=layer@data$knotFillColour2[[loopIndex(i,length(layer@data$knotFillColour2))]]
			answer@components[[5*i-1]]=new("component",type="celticKnotFill",border=NA,x=c(),y=c(),col=sectionColours)
			answer@components[[5*i]]=new("component",type="celticKnotFill",border=NA,x=c(),y=c(),col=sectionColours2)

			for(j in 1:length(sectionColours)){
				index2=ceiling((j/length(sectionColours))*length(layer@components[[3*i]]@data$x))
				index1=ceiling(((j-1)/length(sectionColours))*length(layer@components[[3*i]]@data$x))
				if(j==1){index1=1}
				newXside1=layer@components[[3*i-1]]@data$x[index1:index2]
				newXside2=layer@components[[3*i]]@data$x[index2:index1]
				newXside3=layer@components[[3*i-2]]@data$x[index1:index2]
				newYside1=layer@components[[3*i-1]]@data$y[index1:index2]
				newYside3=layer@components[[3*i-2]]@data$y[index1:index2]
				newYside2=layer@components[[3*i]]@data$y[index2:index1]
				answer@components[[5*i]]@data$x=c(answer@components[[5*i]]@data$x,newXside1,newXside2)
				answer@components[[5*i]]@data$y=c(answer@components[[5*i]]@data$y,newYside1,newYside2)
				answer@components[[5*i-1]]@data$x=c(answer@components[[5*i-1]]@data$x,newXside3,newXside2)
				answer@components[[5*i-1]]@data$y=c(answer@components[[5*i-1]]@data$y,newYside3,newYside2)
				if(j<length(sectionColours)){
						answer@components[[5*i]]@data$x=c(answer@components[[5*i]]@data$x,NA)
						answer@components[[5*i]]@data$y=c(answer@components[[5*i]]@data$y,NA)
						answer@components[[5*i-1]]@data$x=c(answer@components[[5*i-1]]@data$x,NA)
						answer@components[[5*i-1]]@data$y=c(answer@components[[5*i-1]]@data$y,NA)
				}
			}
		}
		answer@data$numPiecesPerSection=5
		return(answer)
	}
}

#' @rdname celticKnots
#' @export
reorderCelticKnotComponents=function(layer){
	answer=layer
	for(i in 1:layer@data$numPiecesPerSection){		answer@components[(1:length(layer@data$knotOrder))*layer@data$numPiecesPerSection+(i-layer@data$numPiecesPerSection)]=layer@components[layer@data$knotOrder*layer@data$numPiecesPerSection+(i-layer@data$numPiecesPerSection)]
	}
	return(answer)
}

#' @rdname celticKnots
#' @export
makeCelticCurve=function(lineOrder=c("A","D","H","L","M","I","F","B","C","G","K","N","J","E"),
		angles=data.frame(angleNumber=c(1:5,7:12,14),inAngle1=pi*c(1/2,1/4,0,-1/4,1,3/4,-1/2,-1/4,0,1/4,1,-3/4),inAngle2=pi*c(1/4,0,-1/4,-1/2,3/4,1,-1/4,0,1/4,1/2,-3/4,1),stringsAsFactors=FALSE),
		basis=data.frame(names=LETTERS[1:14],x=c(1,1,3,3,5,5,6,6,7,7,9,9,11,11),y=c(1,7,3,5,3,5,1,7,3,5,3,5,1,7),stringsAsFactors=FALSE),
		knotOrder=c(1,3,5,7,9,11,13,2,4,6,8,10,12,14),knotFillColour=list("white"),knotBorderColour="black",
		clockWiseCorners=c(1:5,8:12),flipCorners=c(13,14),distance=0.2,knotMode="DEFAULT",...){
		answer=new("layer")+artdata(lineOrder=lineOrder,angles=angles,basis=basis,knotOrder=knotOrder,distance=distance,
			knotFillColour=knotFillColour,celticKnotBorder_.col=knotBorderColour,
			clockWiseCorners=clockWiseCorners,flipCorners=flipCorners,knotMode=knotMode,...)+action.object(c("startCelticRibbon","celticBezierCurve","loopToRibbon","reorderCelticKnotComponents"),...)
		return(answer)
}
