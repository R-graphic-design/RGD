#' functionList
#'
#' @description
#' An S4 class for storing functions for components, layers, sections and so on.
#' @slot functionList a list of functions. These can be names or actual functions.
#' @param functionList a list passed to the functionList slot
#' @param ... extra items to be added to the functionList slot
#' @details
#' functionList is the recommended way to add functions to art objects inside a make function.
#' Functions like action.data are another way to do this.
#' @export
#' @examples
#' print(1+1)
#' @name functionList
setClass("functionList",slots=c(functionList="ANY",frames="integerSet",abcd="character",repeats="numeric",useData="logical",type="character"))

#' @rdname functionList
#' @method initialise functionList
#' @export
setMethod("initialize","functionList",function(.Object,...,functionList=as.character(),frames=integerSet(),abcd="a",repeats=1,useData=TRUE,type=""){
	.Object@functionList=c(functionList,...)
	.Object@frames=frames
	.Object@abcd=abcd
	.Object@repeats=repeats
	.Object@useData=useData
	.Object@type=type
})

#' @rdname functionList
#' @export
functionList=function(functions=c(),...){
	#STEP 1 -> GET PASSED VALUES
	.frames=list(...)$.frames
	.abcd=list(...)$.abcd
	.repeats=list(...)$.repeats
	.useData=list(...)$.useData
	.type=list(...)$.type
	#STEP 2 -> SET DEFAULTS AND RESOLVE SHORTCUTS
	#type can stay null to represent undefined.
	if(is.null(.repeats)){.repeats=1}
	if(is.null(.frames)){.frames=integerSet()}
	if(class(.frames)=="list"){
		for(i in seq_along(.frames)){
			if(class(.frames[[i]])=="numeric"){.frames[[i]]=frames(.frames[[i]])}
		}
	}
	if(class(.frames)=="numeric"){
		.frames=list(frames(.frames))
	}
	if(class(.frames)=="integerSet"){
		.frames=list(.frames)
	}
	if(is.null(.abcd)){.abcd="a"}
	if(is.null(.useData)){.useData=TRUE}
	if(class(functions)=="character"){functions=as.list(functions)}
	#RETURN THE ANSWER	
	return(new("functionList",functionList=functions,frames=.frames,abcd=.abcd,repeats=.repeats,useData=.useData,type=.type))
}
#' @rdname functionList
#' @export
setMethod("+",c("component","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	displayToAdd=list()
	for(i in 1:length(e2@functionList)){
		newFun=componentfunction(fun=e2@functionList[[i]],
									repeats=loopSubset(e2@repeats,i),
									returnComponents=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(e1@action,actionsToAdd)}
	if(length(buildToAdd)>0){e1@build=c(e1@build,buildToAdd)}
	if(length(displayToAdd)>0){e1@display=c(e1@display,displayToAdd)}
	return(e1)}
)
#' @rdname functionList
#' @export
setMethod("-",c("component","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	displayToAdd=list()
	for(i in 1:length(e2@functionList)){
		newFun=componentfunction(fun=e2@functionList[[i]],
									repeats=loopSubset(e2@repeats,i),
									returnComponents=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(actionsToAdd,e1@action)}
	if(length(buildToAdd)>0){e1@build=c(buildToAdd,e1@build)}
	if(length(displayToAdd)>0){e1@display=c(displayToAdd,e1@display)}
	return(e1)}
)

#' @rdname functionList
#' @export
setMethod("+",c("layer","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	displayToAdd=list()
	newFun=""
	for(i in 1:length(e2@functionList)){
		if(!is.null(e2@type)&&e2@type=="component"){
			newFun=componentfunction(fun=e2@functionList[[i]],
										repeats=loopSubset(e2@repeats,i),
										returnComponents=!loopSubset(e2@useData,i),
										frames=loopSubset(e2@frames,i))
			}else{
			newFun=layerfunction(fun=e2@functionList[[i]],
									returnLayer=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
			
		}
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(e1@action,actionsToAdd)}
	if(length(buildToAdd)>0){e1@build=c(e1@build,buildToAdd)}
	if(length(displayToAdd)>0){e1@display=c(e1@display,displayToAdd)}
	return(e1)}
)

#' @rdname functionList
#' @export
etMethod("-",c("layer","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	displayToAdd=list()
	newFun=""
	for(i in 1:length(e2@functionList)){
		if(!is.null(e2@type)&&e2@type=="component"){
			newFun=componentfunction(fun=e2@functionList[[i]],
										repeats=loopSubset(e2@repeats,i),
										returnComponents=!loopSubset(e2@useData,i),
										frames=loopSubset(e2@frames,i))
			}else{
			newFun=layerfunction(fun=e2@functionList[[i]],
									returnLayer=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
			
		}
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(actionsToAdd,e1@action)}
	if(length(buildToAdd)>0){e1@build=c(buildToAdd,e1@build)}
	if(length(displayToAdd)>0){e1@display=c(displayToAdd,e1@display)}
	return(e1)}
)


#' @rdname functionList
#' @export
setMethod("+",c("section","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	cameraToAdd=list()
	displayToAdd=list()
	newFun=""
	for(i in 1:length(e2@functionList)){
		if(!is.null(e2@type)&&e2@type=="component"){
			newFun=componentfunction(fun=e2@functionList[[i]],
										repeats=loopSubset(e2@repeats,i),
										returnComponents=!loopSubset(e2@useData,i),
										frames=loopSubset(e2@frames,i))
			}
		if(!is.null(e2@type)&&e2@type=="layer"){
			newFun=layerfunction(fun=e2@functionList[[i]],
									returnLayer=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))			
		}
		if(is.null(e2@type)||e2@type=="section"){
			newFun=sectionfunction(fun=e2@functionList[[i]],		
									returnSection=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
		}
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="c"){
			cameraToAdd=c(cameraToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(e1@action,actionsToAdd)}
	if(length(buildToAdd)>0){e1@build=c(e1@build,buildToAdd)}
	if(length(cameraToAdd)>0){e1@camera=c(e1@camera,cameraToAdd)}
	if(length(displayToAdd)>0){e1@display=c(e1@display,displayToAdd)}
	return(e1)}
)



#' @rdname functionList
#' @export
setMethod("-",c("section","functionList"),function(e1,e2){
	actionsToAdd=list()
	buildToAdd=list()
	cameraToAdd=list()
	displayToAdd=list()
	newFun=""
	for(i in 1:length(e2@functionList)){
		if(!is.null(e2@type)&&e2@type=="component"){
			newFun=componentfunction(fun=e2@functionList[[i]],
										repeats=loopSubset(e2@repeats,i),
										returnComponents=!loopSubset(e2@useData,i),
										frames=loopSubset(e2@frames,i))
			}
		if(!is.null(e2@type)&&e2@type=="layer"){
			newFun=layerfunction(fun=e2@functionList[[i]],
									returnLayer=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))			
		}
		if(is.null(e2@type)||e2@type=="section"){
			newFun=sectionfunction(fun=e2@functionList[[i]],		
									returnSection=!loopSubset(e2@useData,i),
									frames=loopSubset(e2@frames,i))
		}
		if(is.null(e2@type)||loopSubset(e2@type,i)=="a"){
			actionsToAdd=c(actionsToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="b"){
			buildToAdd=c(buildToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="c"){
			cameraToAdd=c(cameraToAdd,newFun)
		}
		if(!is.null(e2@type)&&loopSubset(e2@type,i)=="d"){
			displayToAdd=c(displayToAdd,newFun)
		}
	}
	if(length(actionsToAdd)>0){e1@action=c(actionsToAdd,e1@action)}
	if(length(buildToAdd)>0){e1@build=c(buildToAdd,e1@build)}
	if(length(cameraToAdd)>0){e1@camera=c(cameraToAdd,e1@camera)}
	if(length(displayToAdd)>0){e1@display=c(displayToAdd,e1@display)}
	return(e1)}
)
