#' easyPlot
#'
#' @description
#' Plots any art object and accepts parameters for a quick way to plot and modify art objects. 
#' @param art either a component, a list of components, a layer, a list of layers, a section, a page or an artwork.
#' @param border size of border(s)
#' @param useGrid logical
#' @param format string
#' @param width,height numerical
#' @param name string
#' @param folder string
#' @param frameFolder string
#' @param xlim,ylim numerical
#' @param widthUnit,heightUnit list of units
#' @param deviceSettings list of settings
#' @param bg background colour
#' @param borderLine border colour
#' @param preserveAspectRatio logical. see addBorder
#' @param .aspectRatio numerical #why .? see addBorder
#' @param plotArt logical. If FALSE simply returns the artwork.
#' @param mode numeric.
#' @param frames numeric.
#' @param framesToPlot numeric.
#' @param framerate numeric.
#' @param innerMargin numeric. see addBorder 
#' @param ... parameters passed to the final artwork using art=art+artdata(...)
#' @return depends on plotArt and mode and art@mode.
#' @export
#' @examples
#' print(1+1)
easyPlot=function(art,border=0.1,useGrid=FALSE,format="screen",width=NULL,height=NULL,
	name=NULL,folder=NULL,frameFolder=NULL,xlim=NULL,ylim=NULL,widthUnit="in",heightUnit="in",
	deviceSettings=list(res=150,antialias="cleartype"),bg=NA,borderLine="black",preserveAspectRatio=FALSE,.aspectRatio=1,
	plotArt=TRUE,mode=NULL,frames=1,framesToPlot=NULL,framerate=c(10,30),innerMargin=0.04,...){
	plotFromLayer=FALSE
	if(class(art)=="component"){art=layer(art)}
	if(class(art)=="list"&&class(art[[1]])=="component"){art=layer(components=art)}
	if(class(art)=="layer"){
		plotFromLayer=TRUE
		art=section(art,
			units=list(x="native",x0="native",x1="native",y="native",y0="native",y1="native",
				xleft="native",xright="native",ytop="native",ybottom="native"))
	}
	if(class(art)=="list"&&class(art[[1]])=="layer"){
		plotFromLayer=TRUE
		art=section(layers=art,
			units=list(x="native",x0="native",x1="native",y="native",y0="native",y1="native",
				xleft="native",xright="native",ytop="native",ybottom="native"))
	}	
	if(class(art)=="section"){
		if(is.null(framesToPlot)){
			framesToPlot=frames
		}
		art=page.new(art,frames=frames,framesToPlot=framesToPlot)
	}
	if(class(art)=="page"){
		if(is.null(mode)){
			mode=0
		}
		art=artwork(art,deviceSettings=deviceSettings,mode=mode)
	}
	art=art+artdata(...)
	art=art-defaultArtSettings()
	art@format=format
	art@framerate=framerate
	if(!is.null(width)){
		art@width=width
		if(format%in%c("png","jpg","bitmap")&widthUnit=="in"){
			art@width=width*deviceSettings$res
		}
	}
	if(!is.null(height)){
		art@height=height
		if(format%in%c("png","jpg","bitmap")&heightUnit=="in"){
			art@height=height*deviceSettings$res
			}
	}
	if(!is.null(folder)){
		art@folder=folder
	}
	if(!is.null(mode)){
		art@mode=mode
	}
	if(!is.null(name)){
		print(name)
		art@name=name[1]
		for(i in 1:length(name)){
			art@pages[[i]]@name=name[i]
		}
	}
	if(!is.null(frameFolder)){
		art@frameFolder=paste(art@folder,frameFolder,sep="//")
	}else{
		art@frameFolder=paste(art@folder,art@name,sep="//")	
	}
	if(art@mode==3&&!dir.exists(art@frameFolder)){dir.create(art@frameFolder)}
	art@useGrid=useGrid
	if(plotFromLayer){
		if(!is.null(xlim)&&!is.null(ylim)){
			art@pages[[1]]@sections[[1]]@xscale=xlim
			art@pages[[1]]@sections[[1]]@yscale=ylim
		}else{
			temp=calculateXYscale(art,preserveAspectRatio,aspectRatio=.aspectRatio,innerMargin=innerMargin)
			art@pages[[1]]@sections[[1]]@xscale=temp$x
			art@pages[[1]]@sections[[1]]@yscale=temp$y
		}
	}
	if(length(border)>0){
		for(i in 1:length(border)){
			art=addBorder(art,extra=border[i],.fill=bg[i],foreground=borderLine[i],mode=art@mode,useGrid=useGrid)
		}
	}
	if(plotArt){
		if(art@mode>-1){
			plotArtwork(art)
		}else{
			return(plotArtwork(art))
		}
	}else{
		return(art)
	}
}