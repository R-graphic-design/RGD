#' makeStringArt
#'
#' @description
#' Functions to calculate the co-ordinates of a piece of string art from a string art object and to make a component to represent a string art object.
#' @param stringArtObject used by runStringArt
#' @param names,x,y,groups,lineGroups,distances,addPoints=FALSE,loop parameters used to create a stringArtObject
#' @param ... parameters passed to the component. See "details" for more information.
#' @details
#' For drawing string art.
#' @family stringArt
#' @return runStringArt returns a list with x0,x1,y0,y1 co-ordinates. makeStringArt returns a component object.
#' @seealso
#' @examples
#' print(1+1)
#' @name makeStringArt
NULL
#> NULL

#' @rdname makeStringArt
#' @export
runStringArt=function(stringArtObject){
	x0=c()
	x1=c()
	y0=c()
	y1=c()
	for(i in 1:length(stringArtObject@lineGroups)){
		for(j in 2:length(stringArtObject@lineGroups[[i]])){
			fromGroup=stringArtObject@groups[[stringArtObject@lineGroups[[i]][j-1]]]
			
			toGroup=stringArtObject@groups[[stringArtObject@lineGroups[[i]][j]]]
		
			possiblePairs=expand.grid(1:length(fromGroup),1:length(toGroup))
			if(class(stringArtObject@distances)=="numeric"){
				if(stringArtObject@distances[i]>-1){
					if(stringArtObject@loop[i]){
						closePairs=abs(possiblePairs[1]-possiblePairs[2])<=stringArtObject@distances[i]
						#this was farPairs=....[i]-1) removing -1 to fix a bug????
						#this worked. why was it there originally??
						farPairs=abs(possiblePairs[1]-possiblePairs[2])>=(max(length(fromGroup),length(toGroup))-stringArtObject@distances[i])
						possiblePairs=possiblePairs[closePairs|farPairs,]
					}else{
						possiblePairs=possiblePairs[abs(possiblePairs[1]-possiblePairs[2])<=stringArtObject@distances[i],]
					}
				}
			}else{ #LIST
				print("using list distances")
					if(stringArtObject@loop[i]){
						#currently ignores negative distances??
						firstDirection=possiblePairs[(possiblePairs[,1]-possiblePairs[,2])%%(max(length(fromGroup),length(toGroup)))%in%stringArtObject@distances[[i]][stringArtObject@distances[[i]]>=-1],]
						secondDirection=possiblePairs[(possiblePairs[,2]-possiblePairs[,1])%%(max(length(fromGroup),length(toGroup)))%in%-stringArtObject@distances[[i]][stringArtObject@distances[[i]]<0],]
						possiblePairs=rbind(firstDirection,secondDirection)
					}else{
						possiblePairs=possiblePairs[((possiblePairs[,1]-possiblePairs[,2]))%in%stringArtObject@distances[[i]],]
					}
			}
			possiblePairs=data.frame(fromGroup[possiblePairs[,1]],toGroup[possiblePairs[,2]],stringsAsFactors=FALSE)
			possiblePairs=possiblePairs[!possiblePairs[,1]==possiblePairs[,2],]

			fromMatch=match(possiblePairs[,1],stringArtObject@names)
			toMatch=match(possiblePairs[,2],stringArtObject@names)
			x0=c(x0,stringArtObject@x[fromMatch])
			x1=c(x1,stringArtObject@x[toMatch])
			y0=c(y0,stringArtObject@y[fromMatch])
			y1=c(y1,stringArtObject@y[toMatch])

		}
	}
	return(list(x0=x0,x1=x1,y0=y0,y1=y1))
}

#' @rdname makeStringArt
#' @export
makeStringArt=function(type="segments",names,x,y,groups,lineGroups,distances,addPoints=FALSE,loop,...){
	answer=new("component",type=type,
		stringArtObject=new("stringArt",names=names,x=x,y=y,groups=groups,lineGroups=lineGroups,distances=distances,loop=loop),...)+action.data("runStringArt",...)
	if(addPoints){answer@type=c("segments","points")
				answer@p$x=x;answer@p$y=y}
	return(answer)
}