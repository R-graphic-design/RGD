#' plottingArtObjects
#'
#' @description
#' Functions to plot objects from this package.
#' @param art accepts only a single art object
#' @details
#' @return
#' @examples
#' print(1+1)
#' @name plottingArtObjects
NULL
#> NULL

#' @rdname plottingArtObjects
#' @export
plotArtwork=function(art){
	if(length(art@pages)>0){
		framesPlotted=0
		fullName=paste(art@folder,art@name,sep="\\")
		if(art@mode==3){fullName=paste(art@frameFolder,art@name,sep="\\")}
		#STEP 0 for mode=-1
		calculatedXLims=c()
		calculatedYLims=c()
		if(art@mode==-1){
			calculatedXLims=xScaleSection(art@pages[[1]]@sections[[1]])
			calculatedYLims=yScaleSection(art@pages[[1]]@sections[[1]])
		}
		
		for(i in 1:length(art@pages)){
			print(paste("page Number",i))
			numFrames=length(art@pages[[i]]@frames)			
			for(j in 1:numFrames){
				currentFrame=art@pages[[i]]@frames[j]
				#STEP ONE OPEN A DEVICE IF NEEDED
				print(paste("step 1",i,j))
				if((art@mode%in%c(0,1)&&i==1&&j==1)|(art@mode>1)){
					currentFileName=fullName
					if(art@mode>1){currentFileName=sprintf(paste(fullName,"_%05d",sep=""),framesPlotted+1)}
					newDeviceSettings=inheritParameters(art@deviceSettings,art@pages[[i]]@deviceSettings,classes=list(),useGrid=art@useGrid)
					
					if(is.null(newDeviceSettings$res)){newDeviceSettings$res=150}
					if(is.null(newDeviceSettings$antialias)){newDeviceSettings$antialias="cleartype"}
					if(is.null(newDeviceSettings$units)){newDeviceSettings$units="px"}
					#if(is.null(newDeviceSettings$fileWidth)){newDeviceSettings$antialias="cleartype"}
					
					###ADD FILE SIZE SHENANIGANS HERE
					
					
					if(art@format=="pdf"){
						if(class(art@fonts)=="character"){
							pdf(file=paste(currentFileName,".pdf",sep=""),width=art@width,height=art@height,fonts=art@fonts)
						}else{
							pdf(file=paste(currentFileName,".pdf",sep=""),width=art@width,height=art@height)				
						}
					}
					if(art@format=="cairoPDF"){
						if(class(art@fonts)=="character"){
							Cairo(file=paste(currentFileName,".pdf",sep=""),width=art@width,height=art@height,fonts=art@fonts,units="in",type="pdf",dpi=72)
						}else{
							Cairo(file=paste(currentFileName,".pdf",sep=""),width=art@width,height=art@height,units="in",type="pdf",dpi=72)				
						}
					}
					if(art@format=="cairoPNG"){
						Cairo(file=paste(currentFileName,".png",sep=""),width=art@width,height=art@height,type="png",units="in",dpi=72)
					}
					if(art@format=="jpg"){
						jpeg(file=paste(currentFileName,".jpg",sep=""),width=art@width,height=art@height,
							res=newDeviceSettings$res,antialias=newDeviceSettings$antialias,units=newDeviceSettings$units)
					}
					if(art@format=="bitmap"){
						bmp(file=paste(currentFileName,".bmp",sep=""),width=art@width,height=art@height,
							res=newDeviceSettings$res,antialias=newDeviceSettings$antialias,units=newDeviceSettings$units)
					}
					if(art@format=="png"){
						png(file=paste(currentFileName,".png",sep=""),width=art@width,height=art@height,
							res=newDeviceSettings$res,antialias=newDeviceSettings$antialias,units=newDeviceSettings$units)
					}
					if(art@format=="screen"){
						dev.new(width=art@width,height=art@height)
					}
					if(art@useShowText){
						if(length(art@fonts)>0){
							for(j in 1:length(art@fonts)){
								if(class(art@fonts[[j]])=="showtextfont"){
									getShowTextFont(art@fonts[[j]])
								}
							}
						}
						showtext_begin()
					}
				}
				#STEP 2 STARTING A PLOT
				print(paste("step 2",i,j))
				checkFrameToPlot=currentFrame%in%art@pages[[i]]@framesToPlot
				checkFirstFrameToPlot=currentFrame==art@pages[[i]]@framesToPlot[1]
				if((art@mode==0&&i==1&&checkFirstFrameToPlot)|(art@mode==1&&checkFrameToPlot)|(art@mode>1&&checkFrameToPlot)){
					oldpar=c()
					if(!art@useGrid){
						settings=newDeviceSettings
						style=art@style
						if(!"xlim"%in%names(settings)){
							if(!"xaxs"%in%names(style)){style$xaxs="i"}
							if(style$xaxs=="i"){
								settings$xlim=c(0,art@width)
							}else{
								settings$xlim=c(-art@width*0.04,art@width*1.04)
							}
						}
						if(!"ylim"%in%names(settings)){
								if(!"yaxs"%in%names(style)){style$yaxs="i"}
								if(style$yaxs=="i"){
									settings$ylim=c(0,art@height)
								}else{
									settings$ylim=c(-art@height*0.04,art@height*1.04)
								}
							}
						oldpar<-par(calibrateParameters(style,useGrid=FALSE))
						plot.new()
						#print(c(xlim=settings$xlim,ylim=settings$ylim))
						plot.window(xlim=settings$xlim,ylim=settings$ylim)#remove xlim,ylim let user use it? NO NEED xlim,ylim...
					}else{
						#plot.new()
						pushViewport(viewport(gp=calibrateParameters(art@style,useGrid=TRUE)))
					}
					vpInfo=list(xinches=c(0,dev.size("in")[1]),yinches=c(0,dev.size("in")[2]),xusr=par("usr")[1:2],yusr=par("usr")[3:4],xnpc=0:1,ynpc=0:1)
				}
				#STEP 3 runningFunctions
				print(paste("step 3",i,j))
				art@pages[[i]]=runPageFunctions(art@pages[[i]],frameNumber=currentFrame,abcd="a")
				if(art@mode==-1){
					calculatedXLims=xScaleSection(art@pages[[1]]@sections[[1]],calculatedXLims[1],calculatedXLims[2])
					calculatedYLims=yScaleSection(art@pages[[1]]@sections[[1]],calculatedYLims[1],calculatedYLims[2])
				}
				#STEP 4 plot if necessary
				print(paste("step 4",i,j))
				checkLastFrameToPlot=(currentFrame==art@pages[[i]]@framesToPlot[length(art@pages[[i]]@framesToPlot)])
				if(checkFrameToPlot){
					temp=runPageFunctions(art@pages[[i]],frameNumber=currentFrame,abcd="b")
					temp=runPageFunctions(temp,frameNumber=currentFrame,abcd="c")
					temp=runPageFunctions(temp,frameNumber=currentFrame,abcd="d")
					if(art@mode==-1){
						calculatedXLims=xScaleSection(art@pages[[1]]@sections[[1]],calculatedXLims[1],calculatedXLims[2])
						calculatedYLims=yScaleSection(art@pages[[1]]@sections[[1]],calculatedYLims[1],calculatedYLims[2])
					}
					if(art@mode>-1){
						plotPageSingleFrame(temp,data=art@data,units=art@units,classes=art@classes,vp=vpInfo,useGrid=art@useGrid,frameNumber=currentFrame)
					}
					#STEP 4B closing a device if necessary
					if((art@mode%in%c(0,1)&&i==length(art@pages)&&checkLastFrameToPlot)|(art@mode>1)){
						if(art@useShowText){
							showtext_end()
						}
						if(!art@useGrid){
							par(oldpar)
						}else{
							popViewport()
						}
						if(art@format!="screen"){
							dev.off()
						}
					}
					framesPlotted=framesPlotted+1
				}
			}#end loop frames
		}#end loop pages
		#STEP 5 animate if necessary
		if(art@mode==3){
			animateCommand=paste("ffmpeg -y -r ",2," -start_number 1 -i ",
						art@frameFolder,"\\",art@name,"_%05d.",art@format,
						" -c:v libx264 -r ",30," -pix_fmt yuv420p ",
						art@folder,"\\",art@name,".mp4",sep="")
			system(animateCommand)
		}
	}#endif check numPages>0
	if(art@mode==-1){return(list(x=calculatedXLims,y=calculatedYLims))}
	if(art@mode==-2){return(art)}
}

#' @rdname plottingArtObjects
#' @export
#OBSOLETE ????
#NOT UPDATED
plotPageAllFrames=function(input,data=list(),units=list(),classes=c(),vp=list(),useGrid=FALSE){
	for(i in input@frames){
		#print(paste("frame=",i))
		input=runPageFunctions(input,frameNumber=i)
		if(i%in%input@framesToPlot){
			temp=runPageFunctions(input,frameNumber=i,plotting=TRUE)
			plotPageSingleFrame(temp,data=data,units=units,classes=classes,vp=vp,useGrid=FALSE,frameNumber=i)
		}
	}
}

#' @rdname plottingArtObjects
#' @export
plotPageSingleFrame=function(input,data,units,classes,vp,useGrid,frameNumber){
	newData=inheritParameters(newData,input@data,classes=classes,useGrid=useGrid)
	newUnits=inheritParameters(units,input@units,classes=classes,useGrid=useGrid)
	
	oldpar=c()
	if(!useGrid){
		oldpar<-par(calibrateParameters(input@style,useGrid=FALSE))
	}else{
		pushViewport(viewport(gp=calibrateParameters(input@style,useGrid=TRUE)))#is this enough to apply settings to the whole page??
	}
	if(length(input@sections)>0){
		for(j in 1:length(input@sections)){
			if(integerCheck(frameNumber,input@sections[[j]]@frames)){
				plotSection(input@sections[[j]],data=newData,units=newUnits,classes=classes,vp=vp,useGrid=useGrid,frameNumber=frameNumber)
			}
		}
	}
	if(!useGrid){
		par(oldpar)
	}else{
		popViewport()
	}	
}

#' @rdname plottingArtObjects
#' @export
plotSection=function(input,data=list(),units=list(),classes=as.character(),vp=list(x=0,y=0),useGrid=TRUE,frameNumber=1){
numLayers=length(input@layers)
numSections=length(input@sections)
if(useGrid){
	#THIS NEEDS CHECKING..
currentViewport=viewport(x =unit(input@x,input@units$x), 
		 y =unit(input@y,input@units$y),
         width = unit(input@width,input@units$width), 
		 height = unit(input@height,input@units$height),
         just = input@just,
		 clip = input@clip,
         xscale = input@xscale, yscale = input@yscale,
         angle = 0)#input@angle)
	pushViewport(currentViewport)
}
	newData=inheritParameters(data,input@data,classes=classes)
	newUnits=inheritParameters(units,input@units,classes=classes)
	
	#ASSUMES just =center
	
	newVP=vp #EVENTUALLY CHANGE TO list()?? no keep width of an inch info??
	#if(attr(input@x,"unit")=="in"){ newVP$x=vp$x+vp$widthOfAnInch*attr(input@x,"valid.unit") }
	#	if(attr(input@x,"unit")=="cm"){ newVP$x=vp$x+vp$widthOfAnInch*attr(input@x,"valid.unit")*0.3937 }

	if(is.unit(input@x)){input@x=list(input@x)}
	if(is.unit(input@y)){input@y=list(input@y)}
	if(is.unit(input@width)){input@width=list(input@width)}
	if(is.unit(input@height)){input@height=list(input@height)}
	if(!useGrid){
		vpCenters=list(xnpc=rep(0,length(input@x)),xusr=rep(0,length(input@x)),xinches=rep(0,length(input@x)),ynpc=rep(0,length(input@y)),yinches=rep(0,length(input@y)),yusr=rep(0,length(input@y)))
		vpWidths=list(xnpc=rep(0,length(input@width)),xusr=rep(0,length(input@width)),xinches=rep(0,length(input@width)),ynpc=rep(0,length(input@height)),yinches=rep(0,length(input@height)),yusr=rep(0,length(input@height)))
		
		for(i in 1:length(input@x)){
		if(unitType(input@x[[i]])=="npc"){
			vpCenters$xnpc[i]=(vp$xnpc[2]-vp$xnpc[1])*as.numeric(input@x[[i]])
			vpCenters$xusr[i]=(vp$xusr[2]-vp$xusr[1])*as.numeric(input@x[[i]])
			vpCenters$xinches[i]=(vp$xinches[2]-vp$xinches[1])*as.numeric(input@x[[i]])
		}
		
		if(unitType(input@x[[i]])%in%c("in","inch","inches")){
			vpCenters$xnpc[i]=(as.numeric(input@x[[i]]))/(vp$xinches[2]-vp$xinches[1])
			vpCenters$xusr[i]=vpCenters$xnpc[i]*(vp$xusr[2]-vp$xusr[1])#editing to fix graphs+vp$xusr[1]
			vpCenters$xinches[i]=as.numeric(input@x[[i]])
		}
		if(unitType(input@x[[i]])=="native"){
			vpCenters$xnpc[i]=(as.numeric(input@x[[i]]))/(vp$xusr[2]-vp$xusr[1])
			vpCenters$xinches[i]=vpCenters$xnpc[i]*(vp$xinches[2]-vp$xinches[1])#editing to fix graphs+vp$xinches[1]
			vpCenters$xusr[i]=as.numeric(input@x[[i]])
		}
		}
		for(i in 1:length(input@width)){
		if(unitType(input@width[[i]])=="npc"){
			vpWidths$xnpc[i]=(vp$xnpc[2]-vp$xnpc[1])*as.numeric(input@width[[i]])
			vpWidths$xusr[i]=(vp$xusr[2]-vp$xusr[1])*as.numeric(input@width[[i]])
			vpWidths$xinches[i]=(vp$xinches[2]-vp$xinches[1])*as.numeric(input@width[[i]])
		}
		if(unitType(input@width[[i]])%in%c("in","inch","inches")){
			vpWidths$xnpc[i]=(as.numeric(input@width[[i]]))/(vp$xinches[2]-vp$xinches[1])

			vpWidths$xusr[i]=vpWidths$xnpc[i]*(vp$xusr[2]-vp$xusr[1])
			#vpCenters$xusr[i]=vpCenters$xnpc[i]*(vp$xusr[2]-vp$xusr[1])#editing to fix graphs+vp$xusr[1]
			vpWidths$xinches[i]=as.numeric(input@width[[i]])
		}
		if(unitType(input@width[[i]])=="native"){
			vpWidths$xnpc[i]=(as.numeric(input@width[[i]]))/(vp$xusr[2]-vp$xusr[1])
			vpWidths$xinches[i]=vpWidths$xnpc[i]*(vp$xinches[2]-vp$xinches[1])
			vpWidths$xusr[i]=as.numeric(input@width[[i]])
		}
		}
		for(i in 1:length(input@y)){
		if(unitType(input@y[[i]])=="npc"){
			vpCenters$ynpc[i]=(vp$ynpc[2]-vp$ynpc[1])*as.numeric(input@y[[i]])
			vpCenters$yusr[i]=(vp$yusr[2]-vp$yusr[1])*as.numeric(input@y[[i]])
			vpCenters$yinches[i]=(vp$yinches[2]-vp$yinches[1])*as.numeric(input@y[[i]])
		}
		if(unitType(input@y[[i]])%in%c("in","inch","inches")){
			vpCenters$ynpc[i]=(as.numeric(input@y[[i]]))/(vp$yinches[2]-vp$yinches[1])
			vpCenters$yusr[i]=vpCenters$ynpc[i]*(vp$yusr[2]-vp$yusr[1])#editing to fix graphs +vp$yusr[1]
			vpCenters$yinches[i]=as.numeric(input@y[[i]])
		}
		if(unitType(input@y[[i]])=="native"){
			vpCenters$ynpc[i]=(as.numeric(input@y[[i]]))/(vp$yusr[2]-vp$yusr[1])
			vpCenters$yinches[i]=vpCenters$ynpc[i]*(vp$yinches[2]-vp$yinches[1])#editing to fix graphs+vp$yinches[1]
			vpCenters$yusr[i]=as.numeric(input@y[[i]])
		}
		}
		for(i in 1:length(input@height)){
		if(unitType(input@height[[i]])=="npc"){
			vpWidths$ynpc[i]=(vp$ynpc[2]-vp$ynpc[1])*as.numeric(input@height[[i]])
			vpWidths$yusr[i]=(vp$yusr[2]-vp$yusr[1])*as.numeric(input@height[[i]])
			vpWidths$yinches[i]=(vp$yinches[2]-vp$yinches[1])*as.numeric(input@height[[i]])
		}
		if(unitType(input@height[[i]])%in%c("in","inch","inches")){
			vpWidths$ynpc[i]=(as.numeric(input@height[[i]]))/(vp$yinches[2]-vp$yinches[1])
			vpWidths$yusr[i]=vpWidths$ynpc[i]*(vp$yusr[2]-vp$yusr[1])
			vpWidths$yinches[i]=as.numeric(input@height[[i]])
		}
		if(unitType(input@height[[i]])=="native"){
			vpWidths$ynpc[i]=(as.numeric(input@height[[i]]))/(vp$yusr[2]-vp$yusr[1])
			vpWidths$yinches[i]=vpWidths$ynpc[i]*(vp$yinches[2]-vp$yinches[1])
			vpWidths$yusr[i]=as.numeric(input@height[[i]])
		}	#^^
		}

		  #print("### section STATS")
		  #print(vpCenters$ynpc)
		  #print("##")
		  #print(vpWidths$ynpc)

		vpCenters$xusr=sum(vpCenters$xusr)
		vpCenters$xnpc=sum(vpCenters$xnpc)
		vpCenters$xinches=sum(vpCenters$xinches)
		vpWidths$xusr=sum(vpWidths$xusr)
		vpWidths$xnpc=sum(vpWidths$xnpc)
		vpWidths$xinches=sum(vpWidths$xinches)	
		vpCenters$yusr=sum(vpCenters$yusr)
		vpCenters$ynpc=sum(vpCenters$ynpc)
		vpCenters$yinches=sum(vpCenters$yinches)
		vpWidths$yusr=sum(vpWidths$yusr)
		vpWidths$ynpc=sum(vpWidths$ynpc)
		vpWidths$yinches=sum(vpWidths$yinches)	
		
		
		newVP$xusr=vp$xusr[1]+vpCenters$xusr+c(-vpWidths$xusr*(input@just[1]),vpWidths$xusr*(1-input@just[1]))
		newVP$xinches=vp$xinches[1]+vpCenters$xinches+c(-vpWidths$xinches*(input@just[1]),vpWidths$xinches*(1-input@just[1]))
		newVP$xnpc=vp$xnpc[1]+vpCenters$xnpc+c(-vpWidths$xnpc*(input@just[1]),vpWidths$xnpc*(1-input@just[1]))
		newVP$yusr=vp$yusr[1]+vpCenters$yusr+c(-vpWidths$yusr*(input@just[2]),vpWidths$yusr*(1-input@just[2]))
		newVP$yinches=vp$yinches[1]+vpCenters$yinches+c(-vpWidths$yinches*(input@just[2]),vpWidths$yinches*(1-input@just[2]))
		newVP$ynpc=vp$ynpc[1]+vpCenters$ynpc+c(-vpWidths$ynpc*(input@just[2]),vpWidths$ynpc*(1-input@just[2]))
	}

	  #print("##")
	  #print(newVP)
	  #print("### END section STATS")
	
	
	
	#if(attr(input@x,"unit")=="npc"&attr(input@width,"width")=="npc"){
	#	newVP$xnpc[1]=vp$xnpc[1]+(vp$xnpc[2]-vp$xnpc[1])*attr(input@x,"valid.unit")-(vp$xnpc[2]-vp$xnpc[1])*attr(input@width,"valid.unit")
	#	newVP$xnpc[2]=vp$xnpc[1]+(vp$xnpc[2]-vp$xnpc[1])*attr(input@x,"valid.unit")+(vp$xnpc[2]-vp$xnpc[1])*attr(input@width,"valid.unit")
	#}else{
	#	newVP$x=vp$x+input@x
	#}
	#if(class(input@y)=="unit"){
	#	if(attr(input@y,"unit")=="in"){ newVP$y=vp$y+vp$HeightOfAnInch*attr(input@y,"valid.unit") }
	#	if(attr(input@y,"unit")=="cm"){ newVP$y=vp$y+vp$HeightOfAnInch*attr(input@y,"valid.unit")*0.3937 }
	#	if(attr(input@y,"unit")=="npc"){ newVP$y=vp$y+vp$vpHeightInches*attr(input@y,"valid.unit")}
	#}else{
	#	newVP$y=vp$y+input@y
	#}
	

	if(numLayers>0){
		for(i in 1:numLayers){
			if(input@layers[[i]]@visible&&integerCheck(frameNumber,input@layers[[i]]@frames)){
				plotLayer(input@layers[[i]],useGrid=useGrid,data=newData,units=newUnits,classes=classes,vp=newVP,xscale=input@xscale,yscale=input@yscale,frameNumber=frameNumber)
			}
		}
	}
	if(numSections>0){
		for(i in 1:numSections){
			if(integerCheck(frameNumber,input@sections[[i]]@frames)){
				plotSection(input@sections[[i]],data=newData,units=newUnits,classes=classes,vp=newVP,useGrid=useGrid,frameNumber=frameNumber)#DON'T NEED xscale?yscale???
			}
		}
	}
	if(useGrid){
	popViewport()
	}
}
#' @rdname plottingArtObjects
#' @export
plotLayer=function(layer,useGrid,data=list(),units=list(),classes=as.character(),vp=list(x=0,y=0),xscale=0:1,yscale=0:1,frameNumber=1){
	if(!useGrid){
		oldpar<-par(calibrateParameters(layer@style,useGrid=FALSE))
	}else{
		#THIS NEEDS CHANGING
		pushViewport(viewport(gp=calibrateParameters(layer@style,useGrid=TRUE),xscale=xscale,yscale=yscale))
	}
	newData=inheritParameters(data,layer@data,classes=classes)
	newUnits=inheritParameters(units,layer@units,classes=classes)
	if(layer@visible){
		if(length(layer@components)>0){
			for(i in 1:length(layer@components)){
				if(layer@components[[i]]@visible&&integerCheck(frameNumber,layer@components[[i]]@frames)){
					plotComponent(layer@components[[i]],useGrid=useGrid,data=newData,units=newUnits,classes=classes,vp=vp,xscale=xscale,yscale=yscale)
				}
			}
		}
		#just added this, so layers inside layers get plotted.
		if(length(layer@layers)>0){
			for(i in 1:length(layer@layers)){
				if(layer@layers[[i]]@visible&&integerCheck(frameNumber,layer@layers[[i]]@frames)){
					plotLayer(layer@layers[[i]],useGrid=useGrid,data=newData,units=newUnits,classes=classes,vp=vp,xscale=xscale,yscale=yscale,frameNumber=frameNumber)
				}
			}
		}
	}
	if(!useGrid){
		par(oldpar)
	}else{
		popViewport()
	}
}
#' @rdname plottingArtObjects
#' @export
plotComponent=function(component,useGrid,data=list(),units=list(),classes=as.character(),vp=list(x=0,y=0),xscale=0:1,yscale=0:1){
	newData=inheritParameters(data,component@data,classes=classes,useGrid=useGrid)
	newUnits=inheritParameters(units,component@units,classes=classes,useGrid=useGrid)
	for(i in 1:length(component@type)){
		plotFunction=component@type[i]
		if(!useGrid){
			#switch to base if from grid

			
			#print(names(newData))
			#print(getpar(newData,fun=plotFunction,style=FALSE,classes=classes,vp=vp,units=newUnits,xscale=xscale,yscale=yscale))
			#print("-----")
			for(j in 1:4){
				if(plotFunction%in%names(classes)){plotFunction=classes[[plotFunction]]}
			}
			if(plotFunction=="grid.points"){plotFunction="points"}
			if(plotFunction=="grid.rect"){plotFunction="rect"}
			if(plotFunction=="grid.polygon"){plotFunction="polygon"}
			if(plotFunction=="grid.lines"){plotFunction="lines"}
			if(plotFunction=="grid.segments"){plotFunction="segments"}
			oldpar=c()
			if(length(component@style)>0){oldpar<-par(calibrateParameters(component@style,fun=component@type[i],style=TRUE,classes=classes,useGrid=FALSE))}
			#print(plotFunction)
			do.call(plotFunction,calibrateParameters(newData,fun=component@type[i],style=FALSE,classes=classes,vp=vp,units=newUnits,xscale=xscale,yscale=yscale,useGrid=FALSE))
			if(length(component@style)>0){par(oldpar)}
		}else{
			
			for(j in 1:4){
				if(plotFunction%in%names(classes)){plotFunction=classes[[plotFunction]]}
			}	
			parsToMove=c()
			if(plotFunction=="points"){plotFunction="grid.points";parsToMove=c("col")}
			if(plotFunction=="rect"){plotFunction="grid.rect";parsToMove=c("lwd","fill","col","border")}
			if(plotFunction=="polygon"){plotFunction="grid.polygon";parsToMove=c("lwd","fill","col","border")}
			if(plotFunction=="lines"){plotFunction="grid.lines";parsToMove=c("lwd","col")}
			if(plotFunction=="segments"){plotFunction="grid.segments";parsToMove=c("lwd","col")}
			print(paste("parsToMove",parsToMove))
			if(length(parsToMove)>0){
				patternSearch=paste0("(",paste0(parsToMove,collapse="|"),")$")
				print(paste("patternSearch",patternSearch))
				print(paste("new Data",names(newData)))
				parsToMove=(names(newData))[which(regexpr(text=names(newData),pattern=patternSearch,perl=TRUE)>0)]
			}
			print(paste("parsToMove",parsToMove))
			vpPushed=FALSE
			if(length(component@style)+length(parsToMove)>0){
				pars=calibrateParameters(component@style,fun=component@type[i],classes=classes,useGrid=TRUE,style=TRUE,additions=newData[parsToMove])
				print(paste("pars (push VP if not empty)",length(pars)))
				print(c(xscale,yscale))
				if(length(pars)>0){
					pushViewport(viewport(gp=pars,xscale=xscale,yscale=yscale))
					vpPushed=TRUE
				}	
			}
			print("DEBUG 05_126")
			print(paste("vpPushed",vpPushed))
			do.call(plotFunction,c(calibrateParameters(newData,units=newUnits,fun=plotFunction,classes=classes,useGrid=TRUE,style=FALSE)))
			if(vpPushed){popViewport()}
		}
	}
}
