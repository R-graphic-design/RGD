#' sectionfunctions
#'
#' @description
#' An S4 class for functions that operate on sections or their parameters
#' @slot fun the name of a function or an function.
#' @slot returnSection logical. If TRUE the function in fun accepts and returns sections. If FALSE it accepts parameters like a normal R function and returns a named list of parameters.
#' @slot frames an integerSet object. Controls on which frames the function runs. The default behaviour is that it runs on all frames.
#' @details
#' no convenient ways yet to add sectionfunctions to sections except manually. 
#' @export
#' @examples
#' print(1+1)
#' @name sectionfunctions
setClass("sectionfunction",slots=c(fun="ANY",returnSection="logical",frames="integerSet"))
#' @rdname sectionfunctions
#' @method initialise sectionfunction
#' @export
setMethod("initialize","sectionfunction",function(.Object,fun=function(x){x},returnSection=TRUE,frames=integerSet()){
	.Object@fun=fun
	.Object@returnSection<-returnSection
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@frames<-frames
	.Object
})
#' @rdname sectionfunctions
#' @export
runSectionFunction=function(input,inputFun,frameNumber){
#print("running runSectionFunction")
if(inputFun@returnSection){
	if("frameNumber"%in%names(as.list(args(inputFun@fun)))){
		return(do.call(inputFun@fun,c(list(input),frameNumber=frameNumber)))
	}else{
		return(do.call(inputFun@fun,list(input)))		
	}
}
}


#' @rdname sectionfunctions
#' @export

runSectionFunctions=function(input,classes=list(),getData=FALSE,frameNumber=1,plotting=FALSE){
#print("running runSectionFunctions")
#print(class(input))
if(getData){input=getRequiredDataSection(input)}
	answer=input
	listFuns=answer@functions
	if(!"numeric"%in%sapply(FUN=class,listFuns)){listFuns=c(listFuns,0)}
	for(i in 1:length(listFuns)){
		if(class(listFuns[[i]])=="numeric"){
			if(length(answer@sections)>0){
				for(j in 1:length(answer@sections)){
					answer@sections[[j]]=runSectionFunctions(answer@sections[[j]],frameNumber=frameNumber,plotting=plotting)
				}
			}
			if(length(answer@layers)>0){
				for(j in 1:length(answer@layers)){
					answer@layers[[j]]=runLayerFunctions(answer@layers[[j]],frameNumber=frameNumber,plotting=plotting)
				}
			}
		}else if(class(listFuns[[i]])%in%c("sectionfunction")&&integerCheck(frameNumber,listFuns[[i]]@frames)){
			answer=runSectionFunction(answer,listFuns[[i]],frameNumber=frameNumber)
		}
	}
	return(answer)
}
