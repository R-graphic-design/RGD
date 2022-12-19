#' runningFunctions
#'
#' @description
#' Functions to run all functions for art objects. This includes cascading a function down to objects contained in other objects.
#' @param input a component, layer, section or page
#' @param classes list
#' @param getData logical
#' @param frameNumber numerical
#' @param abcd "a","b","c" or "d". Short for action, build, camera and display respectively.
#' @details
#' Functions to run functions an art object contains and does the same for all sub-objects. Only functions that match the current frame and value of abcd are run.
#' @return
#' @examples
#' print(1+1)
#' @name runningFunctions
NULL

#' @rdname runningFunctions
#' @export
runAllFunctions=function(input,classes=list(),getData=FALSE,frameNumber,abcd="a"){
	if(getData){input=getRequiredDataPage(input)}
	answer=input
	#Do pages first, get them out of the way
	if(class(input)=="page"){
		if(length(answer@sections)>0){
			#run all sections of a layer
			for(i in 1:length(answer@sections)){
				answer@sections[[i]]=runAllFunctions(answer@sections[[i]],answer@classes,frameNumber=frameNumber,abcd=abcd)
			}
		}
		return(answer)
	}
	#get a list of functions to run
	listFuns=list()
	if(class(input)%in%c("component","layer","section")&&abcd=="a"){listFuns=input@action}
	if(class(input)%in%c("component","layer","section")&&abcd=="b"){listFuns=input@build}
	if(class(input)%in%c("section")&&abcd=="c"){listFuns=input@camera}
	if(class(input)%in%c("component","layer","section")&&abcd=="d"){listFuns=input@display}
	#numerics control when subObjects get processes. By default all sub-processes are done afterwards, but you can put 0/1/2 in action/build/display to change this. components before layers before sections.
	numericsFound=c()
	if(length(listFuns)>0){
		for(i in seq_along(listFuns)){
			if(class(listFuns[[i]])=="numeric"){
				numericsFound=c(numericsFound,listFuns[[i]])
			}
		}
	}
	if(class(input)=="layer"&&!(0%in%numericsFound)){listFuns=c(listFuns,0)}#0=components
	if(class(input)%in%c("layer","section")&&!(1%in%numericsFound)){listFuns=c(listFuns,1)}#1=layers
	if(class(input)=="section"&&!(2%in%numericsFound)){listFuns=c(listFuns,2)}#2=sections
	#Components get converted into a list of components
	if(class(input)=="component"){answer=list(answer)}
	#Now run functions
	if(length(listFuns)>0){
		for(i in seq_along(listFuns)){
			if(class(listFuns[[i]])=="numeric"){
				if(listFuns[[i]]==0&&length(answer@components)>0){
					#run all components of a layer
					temp=list()
					for(j in 1:length(answer@components)){
						temp=c(temp,runAllFunctions(answer@components[[j]],answer@classes,frameNumber=frameNumber,abcd=abcd))
					}
					answer@components=temp
				}
				if(listFuns[[i]]==1&&length(answer@layers)>0){
					#run all layers of a layer or section
					for(j in 1:length(answer@layers)){
						answer@layers[[j]]=runAllFunctions(answer@layers[[j]],answer@classes,frameNumber=frameNumber,abcd=abcd)
					}
				}
				if(listFuns[[i]]==2&&length(answer@sections)>0){
					#run all sections of a section
					for(j in 1:length(answer@sections)){
						answer@sections[[j]]=runAllFunctions(answer@sections[[j]],answer@classes,frameNumber=frameNumber,abcd=abcd)
					}
				}
			}else{
				frameSet=new("integerSet") #autopass if function is a character
				if(class(listFuns[[i]])!="character"){frameSet=listFuns[[i]]@frames}
				if(integerCheck(frameNumber,frameSet)){
					cascadeFLAG=TRUE
					if(class(input)=="section"&&class(listFuns[[i]])%in%c("character","sectionfunction")){
						answer=runSectionFunction(answer,listFuns[[i]],frameNumber=frameNumber)
						cascadeFLAG=FALSE
					}
					if(class(input)=="layer"&&class(listFuns[[i]])%in%c("character","layerfunction")){
						answer=runLayerFunction(answer,listFuns[[i]],classes,frameNumber=frameNumber)
						cascadeFLAG=FALSE
					}
					if(class(input)=="component"&&class(listFuns[[i]])%in%c("character","componentfunction")){
						cascadeFLAG=FALSE
						temp=list()
						for(j in seq_along(answer)){
							temp=c(temp,runComponentFunction(answer[[j]],listFuns[[i]],frameNumber=frameNumber))
						}
						answer=temp
					}
					if(cascadeFLAG){
						answer=cascadeFunction(answer,listFuns[[i]],classes=classes,frameNumber=frameNumber)
					}
				}
			}
		}
	}
	return(answer)
}



#' @rdname runningFunctions
#' @export
cascadeFunction=function(input,inputFunction,classes=list(),frameNumber){
		answer=input
		if(class(input)=="layer"){
			if(length(answer@layers)>0){
				for(i in seq_along(answer@layers)){
					answer@layers[[i]]=cascadeFunction(answer@layers[[i]],inputFunction,classes=classes,frameNumber=frameNumber)
				}
			}
			if(length(answer@components)>0){
				temp=list()
				for(i in seq_along(answer@components)){
					temp=c(temp,runComponentFunction(answer@components[[i]],inputFunction,frameNumber=frameNumber))
				}
				answer@components=temp
			}
			return(answer)
		}
		if(class(input)=="section"){
			if(length(answer@sections)>0){
				for(i in seq_along(answer@sections)){
					answer@sections[[i]]=cascadeFunction(answer@sections[[i]],inputFunction,classes=classes,frameNumber=frameNumber)
				}
			}
			if(class(inputFunction)=="layerfunction" && length(answer@layers)>0){
				for(i in seq_along(answer@layers)){
					answer@layers[[i]]=runLayerFunction(answer@layers[[i]],inputFunction,frameNumber=frameNumber)
				}
			}
			if(class(inputFunction)=="componentfunction" && length(answer@layers)>0){
				for(i in seq_along(answer@layers)){
					answer@layers[[i]]=cascadeFunction(answer@layers[[i]],inputFunction,classes=classes,frameNumber=frameNumber)
				}
			}			
			return(answer)
		}

}