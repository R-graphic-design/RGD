#' Sierpinski Triangle
#'
#' @description
#' Functions to make sierpinski triangles.
#' @param
#' @param
#' @param component a triangle to be split up.
#' @param type the type of component
#' @param ... parameters passed to the component and the component function. See "details" for more information.
#' @details
#' @return runSierpinski returns a list of components. makeSierpinski returns a component.
#' @family fractals
#' @seealso
#' @examples
#' print(1+1)
#' @name sierpinski

#' @rdname sierpinski
#' @export
runSierpinski=function(component){
	answer=list(component,component,component)
	x=component@data$x
	y=component@data$y

	#IMPROVE THIS??
	#add generation support
	answer[[1]]@data$x[2]=(x[1]+x[2])/2
	answer[[1]]@data$x[3]=(x[1]+x[3])/2
	answer[[1]]@data$y[2]=(y[1]+y[2])/2
	answer[[1]]@data$y[3]=(y[1]+y[3])/2
	answer[[2]]@data$x[1]=(x[2]+x[1])/2
	answer[[2]]@data$x[3]=(x[2]+x[3])/2
	answer[[2]]@data$y[1]=(y[2]+y[1])/2
	answer[[2]]@data$y[3]=(y[2]+y[3])/2
	answer[[3]]@data$x[2]=(x[3]+x[2])/2
	answer[[3]]@data$x[1]=(x[3]+x[1])/2
	answer[[3]]@data$y[2]=(y[3]+y[2])/2
	answer[[3]]@data$y[1]=(y[3]+y[1])/2
	if("addRandomness"%in%names(component@data)&&component@data$addRandomness){
		if(is.numeric(component@data$randomness)){
			for(i in 1:length(answer)){
				if(runif(1)>component@data$randomness){
					answer[[i]]@active=FALSE
				}
			}			
		}
	}
	if("addMidpoint"%in%names(component@data)&&component@data$addMidpoint){
		answer[[length(answer)+1]]=component
		answer[[length(answer)]]@data$x=c(x[1]+x[2],x[2]+x[3],x[3]+x[1])/2
		answer[[length(answer)]]@data$x=c(y[1]+y[2],y[2]+y[3],y[3]+y[1])/2
		answer[[length(answer)]]@active=FALSE
	}
	if("addFourthTriangle"%in%names(component@data)&&component@data$addFourthTriangle){
		answer[[length(answer)+1]]=component
		answer[[length(answer)]]@data$x[1]=(x[1]*2+x[2]+x[3])/4
		answer[[length(answer)]]@data$x[2]=(x[2]*2+x[1]+x[3])/4
		answer[[length(answer)]]@data$x[3]=(x[3]*2+x[2]+x[1])/4
		answer[[length(answer)]]@data$y[1]=(y[1]*2+y[2]+y[3])/4
		answer[[length(answer)]]@data$y[2]=(y[2]*2+y[1]+y[3])/4
		answer[[length(answer)]]@data$y[3]=(y[3]*2+y[2]+y[1])/4
	}
	if("areaDeactivate"%in%names(component@data)&&component@data$areaDeactivate>0){
		for(i in 1:length(answer)){
			x=answer[[i]]@data$x
			y=answer[[i]]@data$y
			if(areaTriangle(x[1],x[2],x[3],y[1],y[2],y[3])<component@data$areaDeactivate){
				answer[[i]]@active=FALSE
			}
		}
	}
	return(answer)
}


#' @rdname sierpinski
#' @export
makeSierpinski=function(type="polygon",...){
	answer=component(type=type,...)+action.object("runSierpinski",...)
	return(answer)
}
