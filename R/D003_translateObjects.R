#' translateObjects
#'
#' @description
#' Functions to manipulate co-ordinate parameters.
#' @param input a component or layer to translate
#' @param translateX,translateY numeric. An amount to translate
#' @param xcoordinates a list of names of parameters considered to be x co-ordinates
#' @param ycoordinates a list of names of parameters considered to be y co-ordinates
#' @return returns a modified version of the input
#' @examples
#' print(1+1)
#' @name translateObjects
NULL
#> NULL

#' @rdname translateObjects
#' @export
#RUN FULL CLASS SYSTEM???
translateComponent=function(input,translateX=0,translateY=0,xcoordinates=coordinateNamesX(),ycoordinates=coordinateNamesY()){
	if(length(translateX)==2){translateY=translateX[2];translateX=translateX[1]}
	if(translateX!=0&length(xcoordinates)>0){
		for(i in xcoordinates){
			if(i%in%names(input@data)){
				input@data[[i]]=input@data[[i]]+translateX
			}
			#translates text_yinches etc even though they aren't in coordinate Names
			functionMatch= gsub(pattern=".+?(?=_)_",x=i,replacement="",perl=TRUE)
			if(length(functionMatch)>0&&functionMatch%in%names(input@data)){
				input@data[functionMatch]=input@data[[functionMatch]]+translateX			
			}
		}
	}
	if(translateY!=0&length(ycoordinates)>0){
		for(i in ycoordinates){
			if(i%in%names(input@data)){
				input@data[[i]]=input@data[[i]]+translateY
			}
			#translates text_yinches etc even though they aren't in coordinate Names
			functionMatch= gsub(pattern=".+?(?=_)_",x=i,replacement="",perl=TRUE)
			if(length(functionMatch)>0&&functionMatch%in%names(input@data)){
				input@data[functionMatch]=input@data[[functionMatch]]+translateY			
			}
		}
	}
	return(input)
}

#' @rdname translateObjects
#' @export
translateLayer=function(input,translateX=0,translateY=0,xcoordinates=coordinateNamesX(),ycoordinates=coordinateNamesY()){
	if(length(translateX)==2){translateY=translateX[2];translateX=translateX[1]}
	#Layer P 
	if(translateX!=0&length(xcoordinates)>0){
		for(i in xcoordinates){
			if(i%in%names(input@data)){
				input@data[[i]]=input@data[[i]]+translateX
			}
			#translates text_yinches etc even though they aren't in coordinate Names
			functionMatch= gsub(pattern=".+?(?=_)_",x=i,replacement="",perl=TRUE)
			if(length(functionMatch)>0&&functionMatch%in%names(input@data)){
				input@data[functionMatch]=input@data[[functionMatch]]+translateX			
			}
		}
	}
	if(translateY!=0&length(ycoordinates)>0){
		for(i in ycoordinates){
			if(i%in%names(input@data)){
				input@data[[i]]=input@data[[i]]+translateY
			}
			#translates text_yinches etc even though they aren't in coordinate Names
			functionMatch= gsub(pattern=".+?(?=_)_",x=i,replacement="",perl=TRUE)
			if(length(functionMatch)>0&&functionMatch%in%names(input@data)){
				input@data[functionMatch]=input@data[[functionMatch]]+translateY			
			}
		}
	}
	#components
	if(length(input@components)>0){
		for(i in 1:length(input@components)){
			input@components[[i]]=translateComponent(input@components[[i]],translateX,translateY,xcoordinates,ycoordinates)
		}
	}
	return(input)
}

