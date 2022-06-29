#' layerfunctions
#'
#' @description
#' An S4 class for functions that operate on layers or their parameters
#' @slot fun the name of a function or an function.
#' @slot returnLayer logical. If TRUE the function in fun accepts and returns layer objects. If FALSE it accepts parameters like a normal R function and returns a named list of parameters.
#' @slot frames an integerSet object. Controls on which frames the function runs. The default behaviour is that it runs on all frames.
#' @details
#' action.layer, action.data, etc... are convenient ways to add layerfunctions to layers. 
#' @export
#' @examples
#' print(1+1)
#' @name layerfunctions
setClass("layerfunction",slots=c(fun="ANY",returnLayer="logical",frames="integerSet"))
#' @rdname layerfunctions
#' @method initialise layerfunction
#' @export
setMethod("initialize","layerfunction",function(.Object,fun=function(x){x},returnLayer=TRUE,frames=integerSet()){
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@fun=fun
	.Object@returnLayer<-returnLayer
	.Object@frames<-frames
	.Object
})
#' @rdname layerfunctions
#' @export
layerfunction=function(...){new("layerfunction",...)}
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
		inputs=c(answer@data,list(style=answer@style),list(visible=answer@visible),list(units=answer@units),list(frames=answer@frames))
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
		answer@data=inheritParameters(answer@data,outputs,classes)
		return(answer)
		}
}
