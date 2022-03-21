#' measuringObjects
#'
#' @description
#' Functions to find the minimum and maximum x or co-ordinates of components, layers or sections.
#' @param input a component, layer or section depending on the function
#' @param cmin, cmax numeric. The current minimum or maximum to compare against. If the co-ordinates in the input do not exceed cmin or cmax, cmin and cmax will be returned instead, unless they are NULL. 
#' @return a vector of length two with the minimum and maximum co-ordinates.
#' @examples
#' print(1+1)
#' @name measuringObjects
NULL
#> NULL
###
### xscale should ignore npc.. currently only works if all co-ordinates are the same scale
###

#' @rdname measuringObjects
#' @export
xScaleComponent=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if("x"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$x,na.rm=TRUE),max(input@data$x,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$x,na.rm=TRUE)),max(answer[2],max(input@data$x,na.rm=TRUE)))
		}
	}
	if("x0"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(c(input@data$x0,na.rm=TRUE,input@data$x1,na.rm=TRUE)),max(c(input@data$x0,na.rm=TRUE,input@data$x1,na.rm=TRUE)))
		}else{
			answer=c(min(answer[1],min(c(input@data$x0,input@data$x1,na.rm=TRUE))),max(answer[2],max(c(input@data$x0,input@data$x1,na.rm=TRUE))))
		}
	}
	if("xleft"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$xleft,na.rm=TRUE),max(input@data$xright,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$xleft,na.rm=TRUE)),max(answer[2],max(input@data$xright,na.rm=TRUE)))
		}
	}
	if("polygon_x"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$polygon_x,na.rm=TRUE),max(input@data$polygon_x,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$polygon_x,na.rm=TRUE)),max(answer[2],max(input@data$polygon_x,na.rm=TRUE)))
		}
	}
	return(answer)
}


#' @rdname measuringObjects
#' @export
yScaleComponent=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if("y"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$y,na.rm=TRUE),max(input@data$y,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$y,na.rm=TRUE)),max(answer[2],max(input@data$y,na.rm=TRUE)))
		}
	}
	if("y0"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(c(input@data$y0,input@data$y1,na.rm=TRUE)),max(c(input@data$y0,input@data$y1,na.rm=TRUE)))
		}else{
			answer=c(min(answer[1],min(c(input@data$y0,input@data$y1,na.rm=TRUE))),max(answer[2],max(c(input@data$y0,input@data$y1,na.rm=TRUE))))
		}
	}
	if("ytop"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$ybottom,na.rm=TRUE),max(input@data$ytop,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$ybottom,na.rm=TRUE)),max(answer[2],max(input@data$ytop,na.rm=TRUE)))
		}
	}
	if("polygon_y"%in%names(input@data)){
		if(is.na(answer[1])){
			answer=c(min(input@data$polygon_y,na.rm=TRUE),max(input@data$polygon_y,na.rm=TRUE))
		}else{
			answer=c(min(answer[1],min(input@data$polygon_y,na.rm=TRUE)),max(answer[2],max(input@data$polygon_y,na.rm=TRUE)))
		}
	}
	return(answer)
}


#' @rdname measuringObjects
#' @export
xScaleLayer=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if(length(input@components)>0){
		answer=xScaleComponent(input@components[[1]])
		if(length(input@components)>1){
			for(i in 2:length(input@components)){
				temp=xScaleComponent(input@components[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	if(length(input@layers)>0){
		if(is.na(answer[1])){
		answer=xScaleLayer(input@layers[[1]])	
		}else{
			temp=xScaleLayer(input@layers[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@layers)>1){
			for(i in 2:length(input@layers)){
				temp=xScaleLayer(input@layers[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	return(answer)
}

#' @rdname measuringObjects
#' @export
yScaleLayer=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if(length(input@components)>0){
		answer=yScaleComponent(input@components[[1]])
		if(length(input@components)>1){
			for(i in 2:length(input@components)){
				temp=yScaleComponent(input@components[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	if(length(input@layers)>0){
		if(is.na(answer[1])){
		answer=yScaleLayer(input@layers[[1]])	
		}else{
			temp=yScaleLayer(input@layers[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@layers)>1){
			for(i in 2:length(input@layers)){
				temp=yScaleLayer(input@layers[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	return(answer)
}

#' @rdname measuringObjects
#' @export
xScaleSection=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if(length(input@layers)>0){
		if(is.na(answer[1])){
		answer=xScaleLayer(input@layers[[1]])	
		}else{
			temp=xScaleLayer(input@layers[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@layers)>1){
			for(i in 2:length(input@layers)){
				temp=xScaleLayer(input@layers[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	if(length(input@sections)>0){
		if(is.na(answer[1])){
		answer=xScaleSection(input@sections[[1]])	
		}else{
			temp=xScaleSection(input@sections[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@sections)>1){
			for(i in 2:length(input@sections)){
				temp=xScaleSection(input@sections[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	return(answer)
}


#' @rdname measuringObjects
#' @export
yScaleSection=function(input,cmin=NULL,cmax=NULL){
	answer=c(NA,NA)
	if(!is.null(cmin)){answer[1]=cmin}
	if(!is.null(cmax)){answer[2]=cmax}
	if(length(input@layers)>0){
		if(is.na(answer[1])){
		answer=yScaleLayer(input@layers[[1]])	
		}else{
			temp=yScaleLayer(input@layers[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@layers)>1){
			for(i in 2:length(input@layers)){
				temp=yScaleLayer(input@layers[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	if(length(input@sections)>0){
		if(is.na(answer[1])){
		answer=yScaleSection(input@sections[[1]])	
		}else{
			temp=yScaleSection(input@sections[[1]])
			answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
		}
		if(length(input@sections)>1){
			for(i in 2:length(input@sections)){
				temp=yScaleSection(input@sections[[i]])
				answer=c(min(answer[1],temp[1]),max(answer[2],temp[2]))
			}
		}
	}
	return(answer)
}
