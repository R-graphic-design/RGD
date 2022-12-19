#' calibrateParameters
#'
#' Takes a list of parameters and tunes them to the requirements. This includes removing shortcuts and calculations for combining units.
#' @param styleList a list of parameters
#' @param simplify logical maybe always TRUE
#' @param fun the function
#' @param style logical determines if the parameters are for par/gpar (TRUE) or a plotting function (FALSE)
#' @param classes classes for plot functions
#' @param vp the co-ordinates for the parent section
#' @param units determines the units for co-ordinates
#' @param xscale,yscale vector of min and max for native units
#' @param useGrid logical
#' @param additions a list of parameters to add to the existing parameter list.
#' @return Returns a named list
#' @export
#' @examples
#' print(1+1)
calibrateParameters=function(styleList,simplify=TRUE,fun=NULL,style=TRUE,classes=as.character(),vp=list(x=0,y=0),units=list(),xscale=0:1,yscale=0:1,useGrid=FALSE,additions=list()){
	answer=styleList
	#if(length(additions)>0){
		#print("DEBUG ADDITIONS 1")
		#print(fun)
		#print(answer)
	#}
	if(length(additions)>0){
		answer=inheritParameters(answer,additions,classes=classes,useGrid=useGrid,applyShortcuts=simplify)
	}
	#if(length(additions)>0){
		#print("DEBUG ADDITIONS 2")
		#print(answer)
	#}
	#############
	## Step One: Simplify shortcuts
	# I think simplify is always TRUE???
	if(simplify){
		answer=applyShortcuts(answer,useGrid=useGrid)
	}
	#if(length(additions)>0){
	#	print("DEBUG ADDITIONS 3")
	#	print(answer)
	#}
	#############
	## Step Two: remove function and class names
	## eg polygon_col="red", col="blue" becomes col="red" IF function is polygon.
	## The same also happens if the name is in classes e.g country="polygon" 
	##		then country_col="green" becomes col="green" and overrides col/polygon_col for countries 
	
	#I moved this plotFUN, is this bad??
	plotFUN=list(fun)
	if(length(answer)>0){
		if(!is.null(fun)){
			if(length(classes)>0){
				for(i in 1:4){
					if(plotFUN[[1]]%in%names(classes)){
						plotFUN=c(classes[[plotFUN[[1]]]],plotFUN)
					}
				}
			}
			replacements=rev(c(paste0(plotFUN[-length(plotFUN)],"_"),list("")))
			for(i in length(plotFUN):1){
				pattern=paste("^",plotFUN[[i]],"_",sep="")
				print("pattern + replacement")
				print(pattern)
				print(replacements[[i]])
				answer=grepRename(answer,pattern,replacements[[i]])
			}
		#names over... doesn't work at all correctly...?
		}
	}
	#if(length(additions)>0){
	#	print("DEBUG ADDITIONS 4")
	#	print(plotFUN)
	#	print(answer)
	#}
	##########################################
	## STEP 3: Removes all arguments you can't pass to the function and aren't relevant to the co-ordinate calculations 

	validNames=validParameters(fun=plotFUN[[1]],simplify=simplify,style=style,useGrid=useGrid)
	if(length(answer)>0){
		answer=answer[names(answer)%in%validNames]
	}
	#if(length(additions)>0){
	#	print("DEBUG ADDITIONS 5")
	#	print(answer)
	#}
	###########################################
	## STEP 4: get data references
	if(length(answer)>0){
		for(i in 1:length(answer)){
			if(class(answer[[i]])[1]=="dataReference"){
				answer[[i]]=getDataFromReference(answer[[i]],list())
			}
		}
	}
	#ONLY HANDLES X AND Y AS SEPERATE THINGS!!
	#print("VP")
	#print(vp)
	#print("END VP")
	#print(paste("xscale",xscale))
	#print(paste("yscale",yscale))
	######################################################
	###	STEP FIVE: calculates the co-ordinates
	##
	##
	usedXCoordTypes=c()
	usedYCoordTypes=c()
	#print(names(answer))
	if(!useGrid){
		for(i in c("x","xleft","xright","x0","x1")){
			if(i%in%names(answer)){
				usedXCoordTypes=c(usedXCoordTypes,i)
				if(i%in%names(units)){
					if(units[[i]]%in%c("in","inches","inch")){ answer[[i]]=vp$xusr[1]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer[[i]]}
					if(units[[i]]=="npc"){ answer[[i]]=vp$xusr[1]+(vp$xusr[2]-vp$xusr[1])*answer[[i]]}
					#print(paste("xscale",xscale))
					if(units[[i]]=="native"){
						answer[[i]]=vp$xusr[1]+(vp$xusr[2]-vp$xusr[1])*(answer[[i]]-xscale[1])/(xscale[2]-xscale[1])
						}
				}else{
					print("WARNING UNITS NOT USED ON X")
					answer[[i]]=answer[[i]]+vp$xusr[1]
				}
			}
		}
		if("xinches"%in%names(answer)){
			for(i in usedXCoordTypes){
				answer[[i]]=answer[[i]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xinches
			}
		}
		if("xinchesxright"%in%names(answer)){
				answer[["xright"]]=answer[["xright"]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xinchesxright
		}
		if("xinchesxleft"%in%names(answer)){
				answer[["xleft"]]=answer[["xleft"]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xinchesxleft
		}
		if("xinchesx0"%in%names(answer)){
				answer[["x0"]]=answer[["x0"]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xinchesx0
		}
		if("xinchesx1"%in%names(answer)){
				answer[["x1"]]=answer[["x1"]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xinchesx1
		}
		if("xvarinches"%in%names(answer)){
				answer[["x"]]=answer[["x"]]+(vp$xusr[2]-vp$xusr[1])/(vp$xinches[2]-vp$xinches[1])*answer$xvarinches
		}
		for(i in c("y","ytop","ybottom","y0","y1")){
			if(i%in%names(answer)){
				usedYCoordTypes=c(usedYCoordTypes,i)
				if(i%in%names(units)){
					if(units[[i]]%in%c("in","inches","inch")){ answer[[i]]=vp$yusr[1]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer[[i]]}
					if(units[[i]]=="npc"){ answer[[i]]=vp$yusr[1]+(vp$yusr[2]-vp$yusr[1])*answer[[i]]}
					if(units[[i]]=="native"){ answer[[i]]=vp$yusr[1]+(vp$yusr[2]-vp$yusr[1])*(answer[[i]]-yscale[1])/(yscale[2]-yscale[1])}
				}else{
							print("WARNING UNITS NOT USED ON Y")
					answer[[i]]=answer[[i]]+vp$yusr[1]
				}
			}
		}
		if("yinches"%in%names(answer)){
			for(i in usedYCoordTypes){
				answer[[i]]=answer[[i]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yinches
			}
		}
		if("yinchesytop"%in%names(answer)){
				answer[["ytop"]]=answer[["ytop"]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yinchesytop
		}
		if("yinchesybottom"%in%names(answer)){
				answer[["ybottom"]]=answer[["ybottom"]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yinchesybottom
		}
		if("yinchesy0"%in%names(answer)){
				answer[["y0"]]=answer[["y0"]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yinchesy0
		}
		if("yinchesy1"%in%names(answer)){
				answer[["y1"]]=answer[["y1"]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yinchesy1
		}
		if("yvarinches"%in%names(answer)){
				print(answer$y)
				answer[["y"]]=answer[["y"]]+(vp$yusr[2]-vp$yusr[1])/(vp$yinches[2]-vp$yinches[1])*answer$yvarinches
				print(answer$y)
		}
	}else{
		for(i in c("x","xleft","xright","x0","x1")){
			if(i%in%names(answer)){
				usedXCoordTypes=c(usedXCoordTypes,i)
				if(i%in%names(units)){
					answer[[i]]=unit(answer[[i]],units[[i]])
				}else{
					answer[[i]]=unit(answer[[i]],"native")
				}
			}
		}
		if("xinches"%in%names(answer)){
			for(i in usedXCoordTypes){
				answer[[i]]=answer[[i]]+unit(answer$xinches,"in")
			}
		}
		if("xinchesxright"%in%names(answer)){
				answer[["xright"]]=answer[["xright"]]+unit(answer$xinchesxright,"in")
		}
		if("xinchesxleft"%in%names(answer)){
				answer[["xleft"]]=answer[["xleft"]]+unit(answer$xinchesxleft,"in")
		}
		if("xinchesx0"%in%names(answer)){
				answer[["x0"]]=answer[["x0"]]+unit(answer$xinchesx0,"in")
		}
		if("xinchesx1"%in%names(answer)){
				answer[["x1"]]=answer[["x1"]]+unit(answer$xinchesx1,"in")
		}
		if("xvarinches"%in%names(answer)){
				answer[["x"]]=answer[["x"]]+unit(answer$xvarinches,"in")
		}
		for(i in c("y","ytop","ybottom","y0","y1")){
			if(i%in%names(answer)){
				usedYCoordTypes=c(usedYCoordTypes,i)
				if(i%in%names(units)){
					answer[[i]]=unit(answer[[i]],units[[i]])
				}else{
					answer[[i]]=unit(answer[[i]],"native")
				}
			}
		}
		if("yinches"%in%names(answer)){
			for(i in usedYCoordTypes){
				answer[[i]]=answer[[i]]+unit(answer$yinches,"in")
			}
		}
		if("yinchesyright"%in%names(answer)){
				answer[["yright"]]=answer[["yright"]]+unit(answer$yinchesyright,"in")
		}
		if("yinchesyleft"%in%names(answer)){
				answer[["yleft"]]=answer[["yleft"]]+unit(answer$yinchesyleft,"in")
		}
		if("yinchesy0"%in%names(answer)){
				answer[["y0"]]=answer[["y0"]]+unit(answer$yinchesy0,"in")
		}
		if("yinchesy1"%in%names(answer)){
				answer[["y1"]]=answer[["y1"]]+unit(answer$yinchesy1,"in")
		}
		if("yvarinches"%in%names(answer)){
				answer[["y"]]=answer[["y"]]+unit(answer$yvarinches,"in")
		}
	}
	#print(paste("x",answer$x))
	#print(paste("y",answer$y))
	#print(par("usr"))
	#if("y"%in%names(answer)){points(answer$x,answer$y)}
	#########################################################################
	## STEP SIX
	## Remove xinches etc.

	answer=answer[!names(answer)%in%validAdjustParameters()]
	if(useGrid&&style){
		#this is incorrect usage, see gparlist function below.
		class(answer)="gpar"
	}
	return(answer)
}