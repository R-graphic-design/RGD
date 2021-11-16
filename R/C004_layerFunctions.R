#' layerfunctions
#'
#' @description
#' An S4 class for functions that operate on layers or their parameters
#' @slot fun the name of a function or an function.
#' @slot returnLayer logical. If TRUE the function in fun accepts and returns layer objects. If FALSE it accepts parameters like a normal R function and returns a named list of parameters.
#' @slot frames an integerSet object. Controls on which frames the function runs. The default behaviour is that it runs on all frames.
#' @slot plotting logical controls whether the function has a temporary effect or permanent during the plotting process.
#' @details
#' p2p_lf() and l2l_lf() are convenient ways to add layerfunctions to layers. 
#' @export
#' @examples
#' print(1+1)
#' @name layerfunctions
setClass("layerfunction",slots=c(fun="ANY",returnLayer="logical",frames="integerSet",plotting="logical"))
#' @rdname layerfunctions
#' @method initialise layerfunction
#' @export
setMethod("initialize","layerfunction",function(.Object,fun=function(x){x},returnLayer=TRUE,frames=integerSet(),plotting=FALSE){
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@fun=fun
	.Object@returnLayer<-returnLayer
	.Object@plotting<-plotting
	.Object@frames<-frames
	.Object
})
#' @rdname layerfunctions
#' @export
runLayerFunction=function(input,inputFun,classes,frameNumber){
#print(inputFun)
if(!class(inputFun)=="layerfunction"){
	inputFun=new("layerfunction",fun=inputFun,returnLayer=FALSE)
}
#print(inputFun)
if(inputFun@returnLayer){
	if("frameNumber"%in%names(as.list(args(inputFun@fun)))){
		return(do.call(inputFun@fun,c(list(input),frameNumber=frameNumber)))
	}else{
		return(do.call(inputFun@fun,list(input)))
	}
}else{
	#print("layer function by parts")
	answer=input
	inputs=c(answer@p,list(style=answer@style),list(visible=answer@visible),list(units=answer@units),list(frames=answer@frames))
	inputs=inputs[names(inputs)%in%names(as.list(args( inputFun@fun )))]
	
	if("frame"%in%names(as.list(args(inputFun@fun)))){
		outputs=do.call(  inputFun@fun , c(inputs,frameNumber=frameNumber))
	}else{
		outputs=do.call(  inputFun@fun , inputs)	
	}
	if("style"%in%names(outputs)){answer@style=outputs$style}
	if("units"%in%names(outputs)){answer@units=outputs$units}
	if("visible"%in%names(outputs)){answer@visible=outputs$visible}
	if("frames"%in%names(outputs)){answer@frames=outputs$frames}
	outputs=outputs[!(names(outputs)%in%c("style","visible","units","frames"))]
	answer@p=inheritParameters(answer@p,outputs,classes)
	return(answer)
	}
}


########################################################
#' @rdname layerfunctions
#' @export
runLayerFunctions=function(input,classes=list(),getData=FALSE,frameNumber,plotting=FALSE){
#print("running runLayerFunctions")


if(getData){input=getRequiredDataLayer(input)}
	answer=input
	listFuns=answer@functions
	numericalFunctions=listFuns[which(sapply(FUN=class,listFuns)=="numeric")]
	if(!0%in%numericalFunctions){listFuns=c(listFuns,0)}#0=componentfunctions
	if(!1%in%numericalFunctions){listFuns=c(listFuns,1)}#1=sublayers not yet implemented or used???
	for(i in 1:length(listFuns)){
		if(class(listFuns[[i]])=="numeric"&&listFuns[[i]]==0){
			#print(paste("RUN LAYER FUNCTIONS TIME STEP",listFuns[[i]]))
			if(length(answer@components)>0){
				tempAnswer=answer
				tempAnswer@components=list()
				for(j in 1:length(answer@components)){
					tempAnswer@components=c(tempAnswer@components,runComponentFunctions(answer@components[[j]],classes=classes,frameNumber=frameNumber,plotting=plotting))
				}
				answer=tempAnswer
			}
		#}else if(class(listFuns[[i]])=="numeric"&&listFuns==1){}#sub layers
		}else if(class(listFuns[[i]])=="componentfunction"&&integerCheck(frameNumber,listFuns[[i]]@frames)&&listFuns[[i]]@plotting==plotting){
			if(length(answer@components)>0){
				#should this apply to all components of sub layers???
				for(j in 1:length(answer@components)){
					answer@components[j]=runComponentFunction(answer@components[[j]],listFuns[[i]],frameNumber=frameNumber)
				}
			}		
		}else if(class(listFuns[[i]])=="layerfunction"&&integerCheck(frameNumber,listFuns[[i]]@frames)&&listFuns[[i]]@plotting==plotting){
			#print("TESTING 1 run layerfunction, class layerfunction")
			answer=runLayerFunction(answer,listFuns[[i]],classes,frameNumber=frameNumber)
		}
	}
	return(answer)
}

