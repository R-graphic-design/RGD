#' inheritParameters
#'
#' Combines parameters from two objects, for example a component and its parent layer.
#' @param parentList,childList lists of parameters. In case of a clash, the childList takes precedence.
#' @param classes list of classes
#' @param useGrid logical. Needed to apply shortcuts.
#' @param applyShortcuts CURRENTLY ALWAYS TRUE??? 
#' @return Returns a list that combines parentList and childList, but resolves any conflicts in favour of the child.
#' @export
#' @examples
#' print(1+1)
inheritParameters=function(parentList,childList,classes=as.character(),applyShortcuts=TRUE,useGrid=FALSE){
	answer=childList
	possibleAdditions=parentList

	if(applyShortcuts){
		answer=applyShortcuts(answer,useGrid=useGrid)	
		possibleAdditions=applyShortcuts(possibleAdditions,useGrid=useGrid)
	}
	confirmedAdditions=c()
	if(length(parentList)>0&length(childList)>0){
		for(i in 1:length(possibleAdditions)){
			candidate=names(possibleAdditions)[i]
			fun=""
			parameter=regmatches(candidate,regexpr(text=candidate,pattern="(?<=_).*$",perl=TRUE))
			if(length(parameter)==0){
				parameter=candidate
			}else{
				fun=regmatches(candidate,regexpr(text=candidate,pattern=".+?(?=_)",perl=TRUE))
			}
			exclusions=c(parameter)
			for(i in 1:4){
				if(fun!=""){
					exclusions=c(exclusions,paste(fun,parameter,sep="_"))
					if(fun%in%names(classes)){
						fun=classes[fun]
					}else{
						fun=""
					}
				}
			}
			if(sum(exclusions%in%names(answer))==0){
				confirmedAdditions=c(confirmedAdditions,possibleAdditions[candidate])
			}
		}
	}else{
		confirmedAdditions=possibleAdditions
	}
	answer=c(answer,confirmedAdditions)
	return(answer)
}
