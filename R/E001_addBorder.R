#' addBorder
#' 
#' @description
#' Adds a border to an artwork object.
#' @param art an artwork object
#' @param extra The size of the border
#' @param res The resolution of the artwork
#' @param foreground colour of line around the border
#' @param mode the mode used in the artwork NEEDED?????
#' @param useGrid logical needed??
#' @param ... parameters are passed to the background component
#' @details creates two additional components, a background colour and a foreground border. Both are rect
#' @return returns an artwork object. The sections of the original are inside new sections that also contain borders.
#' @export
#' @examples
#' print(1+1)
#differences with grid? rect uses x,y,w,h not xl,xr,yb,yt
addBorder=function(art,extra=0.1,res=NULL,foreground="black",mode=0,useGrid=FALSE,...){
	if(is.null(res)){
		if(!is.null(art@deviceSettings$res)){
			res=art@deviceSettings$res
		}else{
			res=150
		}
	}

	print("RUNNING ADD BORDER BASE")
	#First the background
	boxCBG=component(type="artBorderBackground",units=list(ybottom="npc",ytop="npc",xleft="npc",xright="npc"))+artp(ybottom=0,ytop=1,xleft=0,xright=1,...)
	boxLBG=layer(boxCBG)

	#Next the original content
	artSections=list()
	for(i in 1:length(art@pages)){
		artSections[[i]]=c(art@pages[[i]]@sections)
	}

	#rep(list(art@pages[[i]]@sections),length(art@pages))
	#Then the border
	boxLFG=layer()
	if(!is.na(foreground)){
		#boxCFG=new("component",type="artBorderForeground",units=list(ybottom="npc",ytop="npc",xleft="npc",xright="npc"))+artp(ybottom=0,ytop=1,xleft=0,xright=1,.border=foreground)
		boxCFG=component(type="artBorderForeground",units=list(y="npc",x="npc"))+artp(x=c(0,0,1,1),y=c(0,1,1,0),.border=foreground)

		boxLFG=layer(boxCFG)
	}
	boxSFG=new("section",boxLFG)
	for(i in 1:length(art@pages)){
		if((mode==0&&i==length(art@pages))|(mode==1)){
			boxSFG@frames=integerSet(max(art@pages[[i]]@framesToPlot))
		}
		artSections[[i]]=c(artSections[[i]],boxSFG)
	}

	oldHeight=art@height
	oldWidth=art@width

	if(art@format%in%c("screen","pdf","cairoPDF","cairoPNG")){
		art=art+settings(width=art@width+extra*2,height=art@height+extra*2)
		for(i in 1:length(art@pages)){
			if((mode==0&&i==length(art@pages))|(mode==1)){
				boxLBG@frames=integerSet(min(art@pages[[i]]@framesToPlot))
			}
			art@pages[[i]]@sections=list(section(width=unit(oldWidth,"in"),height=unit(oldHeight,"in"),clip="on",
										#xscale=artSections[[i]]@xscale,yscale=artSections[[i]]@yscale,
										sections=artSections[[i]],
										layers=list(boxLBG)))
		}
	}else if(art@format%in%c("png","jpg","bitmap")){
		art=art+settings(width=art@width+extra*2*res,height=art@height+extra*2*res)
		for(i in 1:length(art@pages)){
			if(mode==0&&i==length(art@pages)){
				boxLBG@frames=integerSet(min(art@pages[[i]]@framesToPlot))
			}
			if(useGrid){
				art@pages[[i]]@sections=list(section(width=unit(oldWidth,"in")/res,height=unit(oldHeight,"in")/res,clip="on",
										#xscale=artSections[[i]]@xscale,yscale=artSections[[i]]@yscale,
										sections=artSections[[i]],
										layers=list(boxLBG)))
			}else{
				art@pages[[i]]@sections=list(section(width=unit(oldWidth,"native"),height=unit(oldHeight,"native"),clip="on",
										#xscale=artSections[[i]]@xscale,yscale=artSections[[i]]@yscale,
										sections=artSections[[i]],
										layers=list(boxLBG)))		
			}
		}
	}
	return(art)
}