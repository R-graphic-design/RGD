#' componentfunctions
#'
#' @description
#' An S4 class for functions that operate on components or their parameters
#' @slot fun the name of a function or an function.
#' @slot repeats numeric. The number of times the function runs per frame when it runs.
#' @slot returnComponents logical. If TRUE the function in fun accepts and returns component objects. If FALSE it accepts parameters like a normal R function and returns a named list of parameters.
#' @slot frames an integerSet object. Controls on which frames the function runs. The default behaviour is that it runs on all frames.
#' @details
#' action.data, action.comp, build.data, build.comp, display.data and display.comp are convenient ways to add componentfunctions to components. 
#' @export
#' @examples
#' print(1+1)
#' @name componentfunctions
setClass("componentfunction",slots=c(fun="ANY",repeats="numeric",returnComponents="logical",frames="integerSet"))
#' @rdname componentfunctions
#' @method initialise componentfunction
#' @export
setMethod("initialize","componentfunction",function(.Object,fun=function(x){x},repeats=1,returnComponents=TRUE,frames=integerSet()){
	if(class(frames)=="numeric"){frames=integerSet(frames)}
	.Object@fun=fun
	.Object@repeats<-repeats
	.Object@returnComponents<-returnComponents
	.Object@frames<-frames
	.Object
})
#' @rdname componentfunctions
#' @export
componentfunction=function(...){new("componentfunction",...)}
#' @rdname componentfunctions
#' @export
#gives frame in case functions want to know frame??
runComponentFunction=function(input,inputFun,frameNumber){
	#print("Running runComponentFunction")
	#print(inputFun)
	if(!class(inputFun)=="componentfunction"){
		inputFun=new("componentfunction",fun=inputFun,returnComponents=FALSE)
	}
	#print(inputFun)
	if(inputFun@returnComponents){
		#print("component function component to component")
		answer=list(input)
		#print(lapply(answer[[1]]@p,"class"))
		for(i in 1:inputFun@repeats){
			tempAnswer=list()
			for(j in 1:length(answer)){
				if(answer[[j]]@active){
					if("frameNumber"%in%names(as.list(args(inputFun@fun)))){
						tempAnswer=c(tempAnswer,do.call(inputFun@fun,c(answer[j],frameNumber=frameNumber)))
					}else{
						tempAnswer=c(tempAnswer,do.call(inputFun@fun,answer[j]))
					}
				}else{
					tempAnswer=c(tempAnswer,answer[j])
				}
			}
			answer=tempAnswer
		}
		#print("TESTING MARKER 23")
		return(answer)
	}else{
		#print("component function by parts")
		answer=input
		inputs=c(answer@data,list(type=answer@type),list(style=answer@style),list(visible=answer@visible),list(units=answer@units),list(frames=answer@frames),list(active=answer@active))
		##ADD THIS FOR LAYERS ETC??
		#print(names(inputs))
		if(class(inputFun@fun)=="character"){
			inputs=grepRename(inputs,paste("^",inputFun@fun,"_",sep=""),"")
		}

		outputs=list()
		for(i in 1:inputFun@repeats){
			if(length(outputs)>0){
				for(j in seq_along(outputs)){
					inputs[names(outputs)[j]]=outputs[j]
				}
			}
			inputs=inputs[names(inputs)%in%names(as.list(args( inputFun@fun )))]
			if("frame"%in%names(as.list(args(inputFun@fun)))){inputs=c(inputs,frameNumber=frameNumber)}
			outputs=do.call(  inputFun@fun ,inputs)
		}
		
		##NEW elements here visible??
		
		if("style"%in%names(outputs)){answer@style=outputs$style}
		if("units"%in%names(outputs)){answer@units=outputs$units}
		if("visible"%in%names(outputs)){answer@visible=outputs$visible}
		if("type"%in%names(outputs)){answer@type=outputs$type}
		if("active"%in%names(outputs)){answer@active=outputs$active}
		if("frames"%in%names(outputs)){answer@frames=outputs$frames}
		outputs=outputs[!(names(outputs)%in%c("style","type","visible","units","frames","active"))]
		answer@data=inheritParameters(answer@data,outputs,list(),applyShortcuts=FALSE)
		return(list(answer))
	}
}

#add frames, plotting
#' @rdname componentfunctions
#' @export

runComponentFunctions=function(input,classes=list(),getData=FALSE,frameNumber=0,abcd="a"){
	if(getData){input=getRequiredDataComponent(input)}
	answer=list(input)
	#Build a list of functions to actually run
	inputFuns=list()
	if(abcd=="a"&&length(input@action)>0){
		for(i in 1:length(input@action)){
			if((class(input@action[[i]])=="componentfunction"&&integerCheck(frameNumber,input@action[[i]]@frames))||(class(input@action[[i]])=="character")){
				inputFuns=c(inputFuns,input@action[[i]])
			}
		}
	}
	if(abcd=="b"&&length(input@build)>0){
		for(i in 1:length(input@build)){
			if(class(input@build[[i]])=="componentfunction"&&integerCheck(frameNumber,input@build[[i]]@frames))||(class(input@build[[i]])=="character")){
				inputFuns=c(inputFuns,input@build[[i]])
			}
		}
	}
	if(abcd=="d"&&length(input@display)>0){
		for(i in 1:length(input@display)){
			if((class(input@display[[i]])=="componentfunction"&&integerCheck(frameNumber,input@display[[i]]@frames))||(class(input@display[[i]])=="character")){
				inputFuns=c(inputFuns,input@display[[i]])
			}
		}
	}
	#run the functions selected.
	if(length(inputFuns)>0){
		for(i in 1:length(inputFuns)){
		tempAnswer=list()
			for(j in 1:length(answer)){
				tempAnswer=c(tempAnswer,runComponentFunction(answer[[j]],inputFuns[[i]],frameNumber=frameNumber))
			}
		answer=tempAnswer
		}
	}
	return(answer)
}